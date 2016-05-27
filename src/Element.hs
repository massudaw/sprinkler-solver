{-# LANGUAGE RankNTypes,MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
{-# LANGUAGE TupleSections,DeriveFunctor,DeriveFoldable #-}
module Element where

import TBL.Parser
import Hydraulic
import Data.Maybe
import Backend.Mecha as Mecha
import Backend.DXF
import qualified Position as P
import Control.Lens ((^.),_1)
import Debug.Trace
import Control.Monad.State
import Tee hiding (ktubo)
import Data.Semigroup
import Data.Functor.Identity
import qualified Data.Map as M
import GHC.Stack
import Domains
import Data.Functor.Compose
import Position
import Control.Monad
import Numeric.GSL.Root
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Vector
import Control.Arrow
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as F
import Control.Applicative
import qualified Numeric.AD as AD

import Linear.V3
import Rotation.SO3 hiding (rotM)


water = Fluido 1.0e-3 997 "Água"
air = Fluido 1.983e-5 air700mDensity "Ar"

air700mDensity = 0.9

densityAir  amb = airDensity  (pontoOrvalho amb) (temperaturaAmbiente amb) (pressaoBarometrica amb) (altitude amb)

defAmbient f a
  = Ambient f a 700 25 15 1103 (V2  (16.605621)(-49.3481687))

instance PreSys Element  where
  -- Global Variables
  type Enviroment Element = Ambient
  -- Node Variables
  type NodeDomain Element = V2
  -- Link Variables
  type LinkDomain Element = Identity
  initIter g = (\e -> Iteration  (fmap Compose <$> varsL) (fmap Compose <$> varsN) e g)
    where
      varsN = fst  $ runState (((traverse (traverse (traverse conv  . constrained )))) $ nodesFlow g) 50
      conv (Just i) = return Nothing
      conv Nothing = do
        i <- get
        put 50
        return (Just  i)
      convL (Just i) = Nothing
      convL Nothing = (Just 98)
      varsL = fmap (fmap ((fmap convL . lconstrained ))) $ (fmap (\(i,(_,_,l))-> (i,l)) $  links g)
  constrained (Tee (TeeConfig _ _ (DuctEntry _ _ )) _ ) = V2 Nothing Nothing
  constrained (Tee (TeeConfig _ _ (RectangularEntry _ _ _ )) _ ) = V2 Nothing Nothing
  constrained (Tee (TeeConfig _ _ (RoundEntry _ _  )) _ ) = V2 Nothing Nothing
  constrained (Reservatorio i) = V2 0 Nothing
  constrained (Grelha _ _ _ _ ) = V2 Nothing Nothing
  constrained (Open i ) = V2 Nothing (Just i)
  constrained (Sprinkler (_,k) _ _ _  ) = V2 Nothing Nothing
  constrained i = V2 Nothing 0
  lconstrained i = Identity $ Nothing


airActualPressure as h =  p
  where
    k1=0.190263;
    k2=8.417286E-5;
    p=((as**k1)-(k2*geoPotential h))**(1/k1)

gravitySea  phi = 9.7803253359 *(1+0.00193185265241*(sin phi)^2/(sqrt(1-0.00669437999013*(sin phi)^2)))

gravityHeight lat h = gravitySea lat  - 3.155e-7*h

localGravity am = gravityHeight (geoposition am ^. _x) (altitude am)

geoPotential z =  ((r*z)/(r+z))
  where
   r=6369E3;

airPressure h = p0 * (1 -  g*h/cp/t0)**(cp*mair/r)
  where t0 = 288.15
        p0 = 101.325
        cp = 1007
        g = 9.80665
        mair = 0.0289644
        r =   8.31447

airDensity :: Floating a => a -> a -> a -> a -> a
airDensity  tdew temp pressure height =  rho*100
  where
    rv = 461.4964
    rd = 287.0531
    tk = temp + 273.15
    rho = pd /(rd*tk) + pv/(rv*tk)
    es t = eso /(p t) ^8
    eso = 6.1078

    p t = c0 + t*(c1 + t*(c2 + t*(c3 + t*(c4 + t*(c5 + t*(c6 + t *(c7 + t *(c8 + t *(c9) ) ) ) ) ) ) ))
    pv = es tdew
    pd = airActualPressure pressure  height - pv
    c0 = 0.99999683
    c1 = -0.90826951e-2
    c2 = 0.78736169e-4
    c3 = -0.61117958e-6
    c4 = 0.43884187e-8
    c5 = -0.29883885e-10
    c6 = 0.21874425e-12
    c7 = -0.17892321e-14
    c8 = 0.11112018e-16
    c9 = -0.30994571e-19



instance Coord Element (V3 Double) where
  thisElement [a,b] (Bomba  _ _ ) = (2,)<$> M.fromList [(a,(0,so3 0)),(b,(V3 0 0 0,so3 (V3 0 0 pi)))]
  thisElement [a,b] (Perda  _ ) = (2,)<$> M.fromList [(a,(0,so3 0)),(b,(V3 0 0 0,so3 (V3 0 0 pi)))]
  thisElement [a,b] (Tubo _ c _ ) = (2,)<$> M.fromList [(a,(0,so3 0)),(b,(V3 (c) 0 0,so3 (V3 0 0 pi)) )]
  thisElement [a,b] (Joelho  c _ ) = (2,)<$> M.fromList [(a,(0,so3 0)),(b,(0,so3 (V3 0 0 (pi + opi c) )))]
  thisElement [a,b] (Turn c ) = (0,)<$> M.fromList [(a,(0,so3 0)),(b,(0,so3 (V3 (pi + opi c) 0 0 )))]
  thisElement _ (Tee (TeeConfig [rl,rr]  _ (FanSystemInteraction (Elbow ang  _ _ ) len _ )) _ ) = (2,).(\ (l,j) -> (V3 0 (-l) 0 ,so3 $ fmap opi (V3 0 0 j ))) <$> M.fromList [(rl,(len,0)),(rr,(0,1/2 + ang/360))]
  thisElement l i = (2,). (\j-> (0,so3 $ fmap opi  (V3 0 0 j))) <$> this  (l,i)
    where
      this =  M.fromList .  els
        where
          els (_,(Tee (TeeConfig [rl,br,bl,rr] _  (RoundTee ang  _ _ )) _ ))
            =  [(rl,1/2 - t ),(rr,-t),(br,0),(bl,1/2)]
              where t = ang/pi/2
          els (_,(Tee (TeeConfig [rl,b,rr] _ (RoundTee ang  _ _ ) ) _ ))
            =  [(rl,1/2 - t ),(rr,-t),(b,0)]
              where t = ang/pi/2
          els (_,(Tee (TeeConfig [rl,b,rr]  _ (RectangularTee _ )) _ ))
            =  [(rl,1/2 - t ),(rr,-t),(b,0)]
              where t = 1/4
          els (_,(Tee (TeeConfig [rl,rr]  _ (Elbow ang  _ _ )) _ ))
            =  [(rl,0 ),(rr,1/2+ ang/360)]
              where t = 1/4
          els ([a,b],i)
            =  [(a,0),(b,1/2)]
          els ([a],i)
            =  [(a,0)]
          els i = errorWithStackTrace $ show ("thisElement",i)

pipeElement am v e | v < 0 = negate $ pipeElement am (abs v) e
pipeElement am v (Bomba  ((pn,vn)) (Poly l ) ) = negate $ (*pn) $ (/100)  $foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*(100*v/vn)**p
pipeElement am v e@(Tubo _ _ _)
  = case fluidName $ fluido am of
      "Água" ->  ktubo am joelhos e v
      "Ar" -> darcy e am v
pipeElement am v e@(Joelho  _ _)  = ktubo am joelhos e v
pipeElement am v e@(Perda  _)  = ktubo am joelhos e v
pipeElement am v (Turn _)   = 0


signedFlow :: (Show a,Floating a )=> Grid Element a -> M.Map Int a ->M.Map Int (M.Map Int a)
signedFlow g v = M.fromList $  fmap (\(i,_) ->  (i,) $ M.fromList $  ( sumn $ flipped i $ links g) ++   ((suma $ correct i $ links g)) ) (nodesFlow g)
  where flipped i=  filter (\(_,(h,t,_)) -> h == i )
        correct i= filter (\(_,(h,t,_)) -> t == i )
        suma =  fmap (\(li,_) -> (li,var li v ))
        sumn =  fmap (\(li,_) ->  (li,negate $ var li v))

jacobianContinuity :: (Show a,Ord a,Floating a )=> Grid Element a -> M.Map Int a -> M.Map Int (V2 a) -> [a]
jacobianContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $  nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,(h,t,_)) -> h == i )
        correct i= suma . filter (\(_,(h,t,_)) -> t == i )
        suma =  fmap (\(li,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        nflow i e = var i pm ^. _y
zoneEquations am g pm  =  catMaybes $ uncurry nflow <$>  nodesFlow g
  where
    nflow i e = do
      fl <- genFlow (var i (M.fromList (nodesPosition g)) ^. _1._z) e
      return $ var i pm ^. _x - fl
    genFlow idf (Grelha  _ _ _ _) = Just $ airPressure (traceShowId $ idf + altitude am)
    genFlow idf (Tee  (TeeConfig _ _  (RoundEntry  _ _)) _ ) = Just $ airPressure (idf + altitude am)
    genFlow idf (Tee  (TeeConfig _ _  (DuctEntry _ _)) _ ) = Just $ airPressure (idf + altitude am)
    genFlow idf (Tee  (TeeConfig _ _  (RectangularEntry  _ _ _)) _ ) = Just $ airPressure (idf + altitude am)
    genFlow _ i = Nothing

leakEquations g pm  =  catMaybes $ uncurry nflow <$>  nodesFlow g
  where
    nflow i e = do
      fl <- genFlow (var i pm ^. _x) e
      return $ var i pm ^. _y - fl
    genFlow idf (Sprinkler (_,k) _ _ _) = Just $ if idf <= 0 then negate k*sqrt (abs idf) else k*sqrt(abs idf)
    genFlow _ i = Nothing

fittingsCoefficient am sflow n t = case fluidName (fluido am) of
          "Água" -> (\(ix,l) -> (ix,ktubo am joelhos l (abs $ var ix flowMap   ))) <$> fittingLossesNFPA (fluido am) joelhos flowMap t
          "Ar" -> pressureDrop (fluido am) flowMap (sectionMap t) <$> fittingLosses (fluido am) (ashrae am) flowMap  t
        where
          flowMap = (fmap (\x -> x/1000/60) $ var n  sflow)


-- Generic Solver | Node + Head Method
jacobianNodeHeadEquation :: (Show a,Ord a,Floating a,Real  a) => Ambient a -> Grid Element a -> M.Map Int a ->M.Map Int (V2 a) -> [a]
jacobianNodeHeadEquation am grid  vm nh =  term <$> l
  where
    l = links grid
    sflow = signedFlow grid vm
    fittings n t = case fluidName (fluido am) of
          "Água" -> (\(ix,l) -> (ix,ktubo am joelhos l (abs $ var ix flowMap   ))) <$> fittingLossesNFPA (fluido am) joelhos flowMap t
          "Ar" -> pressureDrop (fluido am) flowMap (sectionMap t) <$> fittingLosses (fluido am) (ashrae am) flowMap  t
        where
          flowMap = (fmap (\x -> x/1000/60) $ var n  sflow)
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t conf ) -> (\(ti,v)-> ((n,ti),v)) <$> fittings n t ) .  filter (isTee .snd) $ nodesFlow grid
    addTee k = maybe 0 id (M.lookup k nodeLosses)
    term (l,(h,t,e)) =   sum (pipeElement am (var l vm) <$> e)  + gravityEffect am (var t nhs ^. _z - var h nhs ^. _z)  + (var t nh ^. _x - var h nh ^. _x )  +  addTee (h,l) + addTee (t,l)
      where
         nhs = fmap fst (M.fromList $ nodesPosition  grid)

gravityEffect am dh = density (fluido am)*dh*(localGravity am)/1000

testEquations = (darcy t am q , ktubo am joelhos t  q)
  where  t = Tubo (Circular 0.315) 1 100
         q = 1000*60
         am = defAmbient water undefined

-- System Equations
reinolds v d f  = traceShowId $ v*d/kinematicViscosity f
-- Multiple fluid equations , input Flow;L/min , output Pressure;kPA

darcy :: (Ord a,Show a,Floating a,Real a ) => Element a -> Ambient a -> a -> a
darcy e flu q = f*l/d*ve^2/2*density (fluido flu)/1000
  where
    l = distanciaE e
    d =  diametroE e
    g = localGravity flu
    vis = kinematicViscosity (fluido flu)
    re = reinolds ve d (fluido flu)
    qm3s = q/1000/60
    ve = qm3s/areaE e
    f
      | re < 4000  = traceShowId $ 64 /re
      | 4000 < re && re <= 10^8 = traceShowId $ colebrook re d (roughnessE e)
      | otherwise = errorWithStackTrace $ "no equation for this condition" <> show (qm3s,ve,vis,re,roughnessE e,d)

-- Aproximate Colebrook Equation
colebrookAprox re dh rough = (6.4/ (log re  - log (1 + 0.01*re*rough /dh *(1+ 10*sqrt(rough /dh))))**2.4)

-- Root of colebrook equation
colebrook :: (Floating a, Real a )=> a -> a -> a -> a
colebrook re dh rough  =  realToFrac $ fst $ uniRootJ Steffenson 1e-8 100   equation (AD.diff equation ) (realToFrac $ colebrookAprox re dh rough )
  where
    equation :: forall a . Floating a => a -> a
    equation = (\k -> 1/sqrt k + 2*logBase 10 (2.51/(realToFrac re*sqrt k) + (realToFrac rough/realToFrac dh)/3.72))


-- Open tube Manning equation
manning t  = perda*10/(1000*60)**1.85
        where
              d = diametroE t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 4.66*(distanciaE t)*c/(d**(16/3))


-- Hazen Willians water equations , input (flow;L/min) ; output (Pressure;KPA )
ktubo env joelhos t q = perda/1000*(density (fluido env)) *(localGravity env)*(q/1000/60)**1.85
  where
    d = diametroE t
    c = materialE t
    -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
    perda = 10.67*(distanciaE t)/(c**1.85)/(d**4.87)

jacobianEqNodeHeadGrid :: (Show a , Ord a ,Floating a,Real a ) => Ambient a -> Grid Element a -> M.Map Int (LinkDomain Element a) -> M.Map Int (NodeDomain Element  a) -> [a]
jacobianEqNodeHeadGrid e = (\l v h -> jacobianNodeHeadEquation e l (runIdentity <$> v) h <> jacobianContinuity l (runIdentity <$> v) h <> leakEquations l h <> zoneEquations e l h )



--------------------------
-- Mecha Backend        --
--------------------------


renderElemMecha   ni (Open i) = Mecha.color (0,1,0,1) $ Mecha.sphere 0.1

renderElemMecha   ni (Reservatorio  i) = (Mecha.color (1,1,1,1) $ Mecha.sphere 0.5 )<>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Grelha _ _ _ _ ) = (Mecha.color (1,1,1,1) $ Mecha.sphere 0.5 )<>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is,os] (Elbow _ _ _ )  ) _ ) = (Mecha.color (0,1,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is,os] (Damper  _ _)  ) _ ) = (Mecha.color (0,1,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is,os] (FanSystemInteraction j@(Elbow ang _ _) len t)  ) _ ) = Mecha.color (1,1,0,1) $ (Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni))) <>  Mecha.rotateZ (pi*ang/180) (renderLinkMecha undefined ni (Tubo os len 100))
renderElemMecha   ni (Tee (TeeConfig _ [is,os] (FireDamper  )  ) _ ) = (Mecha.color (1,0,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is,os] (Transition  _)  ) _ ) = (Mecha.color (0,1,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is,os] (Screen _ _)  ) _ ) = (Mecha.color (0,1,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is] (RectangularEntry _ _ _)  ) _ ) =(Mecha.color (0,1,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is] (RoundEntry  _ _)  ) _ ) = (Mecha.color (0,1,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [is] (DuctEntry  _ _)  ) _ ) =(Mecha.color (0,1,0,1) $ Mecha.sphere (hydraulicDiameter is*1.05)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _  [Circular i ,Circular j,_]  _ ) _ ) = (Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha   ni (Tee (TeeConfig _ [r ,Rectangular i j,_] _  ) _ ) = (Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha   ni (Sprinkler ((d,k)) _ fa@(SPKCoverage sx sy sz (SPKGoods g _ ) ) a ) = (Mecha.sphere 0.15) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))


renderElemMecha   ni i = errorWithStackTrace  $ show ("renderElemMecha",ni,i)

renderLinkMecha   _ ni (Tubo s c _ )
  = case s of
    Circular d -> (Mecha.color (0.2,0.2,1, 1 ) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999))
    Rectangular h w  -> Mecha.color (0.2,0.2,1, 1 ) $ Mecha.scale  (c,w,h) $ Mecha.move (0.5,0,0) $  Mecha.cube 1

renderLinkMecha   nis ni (Joelho _ (TabelaPerda (d)  c _  )  ) = Mecha.sphere (hydraulicDiameter d) -- <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha   nis ni  (Bomba i  v ) = Mecha.moveX (0.03/2) $ Mecha.sphere 0.4 -- <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha   nis _  o = Mecha.sphere 0.02

instance Target Element Mecha.Solid  where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha


----
-- DXF Backend
---

instance Target Element [EntityTy] where
  renderLink  nis ni (Tubo (Circular d) c _ ) = [TEXT (V3 (c/2) 0.3 0) 0.2 (show $ round (d*1000))  Nothing Nothing, LINE 0 (V3 c 0 0)]
  renderLink  nis ni (Joelho _ (TabelaPerda (d) c _ ) ) = []
  renderLink  nis ni i = [CIRCLE 0 0.2]
  renderNode  nis (Sprinkler _ _ _ _)  = [INSERT "spk" 0  (Just 1) Nothing Nothing []]
  renderNode  nis (Tee _ _) = []
  renderNode  nis (Open _ ) = []
  renderNode  nis i = [CIRCLE 0 0.2]



