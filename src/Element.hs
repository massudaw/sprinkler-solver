{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
{-# LANGUAGE TupleSections,DeriveFunctor,DeriveFoldable #-}
module Element where

import Hydraulic
import qualified Position as P
import Control.Lens ((^.))
import Debug.Trace
import Control.Monad.State
import Tee hiding (ktubo)
import Data.Monoid
import Data.Functor.Identity
import qualified Data.Map as M
import GHC.Stack
import Domains
import Data.Functor.Compose
import Position
import Control.Monad
import Linear.V3
import Linear.Matrix
import Linear.Vector
import Control.Arrow
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as F

import Linear.V3
import Rotation.SO3 hiding (rotM)

data Ambient a
  = Ambient
  { fluido :: Fluido a
  , gravidade :: a
  , densidadeAmbiente :: a
  -- , geoposition :: V2 a a a
  }deriving(Eq,Ord,Functor,Show)

water = Fluido 1.0e-3 997 "Água"
air = Fluido 1.983e-5 air700mDensity "Ar"

air700mDensity = 0.9

defAmbient
  = Ambient water 9.81 air700mDensity

instance PreSys Element  where
  type Enviroment Element = Ambient
  type NodeDomain Element = Identity
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

  constrained (Tee (TeeConfig _ _ (DuctEntry _ _ )) _ ) = Identity (Just 0)
  constrained (Tee (TeeConfig _ _ (RectangularEntry _ _ _ )) _ ) = Identity (Just 0)
  constrained (Tee (TeeConfig _ _ (RoundEntry _ _  )) _ ) = Identity (Just 0)
  constrained (Reservatorio i) = Identity (Just 0)
  constrained (Grelha _ _ _ _ ) = Identity (Just 0)
  constrained i = Identity $ Nothing
  lconstrained i = Identity $ Nothing




instance Coord Element (V3 Double) where
  thisElement [a,b] (Bomba  _ _ ) = (2,)<$> M.fromList [(a,(0,SO3 $ P.rotM 0)),(b,(V3 0 0 0,SO3 $ P.rotM 0))]
  thisElement [a,b] (Perda  _ ) = (2,)<$> M.fromList [(a,(0,SO3 $ P.rotM 0)),(b,(V3 0 0 0,SO3 $ P.rotM 0))]
  thisElement [a,b] (Tubo _ c _ ) = (2,)<$> M.fromList [(a,(0,SO3 $ P.rotM 0)),(b,(V3 c 0 0,SO3 $ P.rotM 0))]
  thisElement [a,b] (Joelho  c _ ) = (2,)<$> M.fromList [(a,(0,SO3 $ P.rotM 0)),(b,(0,SO3 $ P.rotM (V3 0 0 (opi c) )))]
  thisElement [a,b] (Turn c ) = (0,)<$> M.fromList [(a,(0,SO3 $ P.rotM 0)),(b,(0,SO3 $ P.rotM (V3 (opi c) 0 0 )))]
  thisElement _ (Tee (TeeConfig [rl,rr]  _ (FanSystemInteraction (Elbow ang  _ _ ) len _ )) _ ) = (2,).(\ (l,j) -> (V3 0 (-l) 0 ,SO3 $ P.rotM $ fmap opi (V3 0 0 j ))) <$> M.fromList [(rl,(len,0)),(rr,(0,1/2 + ang/360))]
  thisElement l i = (2,). (\j-> (0,SO3 $ P.rotM $ fmap opi  (V3 0 0 j))) <$> this  (l,i)
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
          els (_,(Tee (TeeConfig [rl,rr]  _ (FanSystemInteraction (Elbow ang  _ _ ) _ _ )) _ ))
            =  [(rl,0 ),(rr,1/2+ ang/360)]
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
      "Água" ->  (ktubo e)*v**1.85
      "Ar" -> darcy e am v
pipeElement am v e@(Joelho  _ _)  = (ktubo e)*v**1.85
pipeElement am v e@(Perda  _)  = (ktubo e)*v**1.85
pipeElement am v (Turn _)   = 0


signedFlow :: (Show a,Floating a )=> Grid Element a -> M.Map Int a ->M.Map Int (M.Map Int a)
signedFlow g v = M.fromList $  fmap (\(i,_) ->  (i,) $ M.fromList $ ( ( sumn $ flipped i $ links g) ++   ((suma $ correct i $ links g))) ) (nodesFlow g)
  where flipped i=  filter (\(_,(h,t,_)) -> h == i )
        correct i= filter (\(_,(h,t,_)) -> t == i )
        suma =  fmap (\(li,_) -> (li,var li v ))
        sumn =  fmap (\(li,_) ->  (li,negate $ var li v))

jacobianContinuity :: (Show a,Ord a,Floating a )=> Grid Element a -> M.Map Int a -> M.Map Int a -> [a]
jacobianContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $ filter (not . isEntry . snd) $filter (not . isGrelha . snd) $ filter (not . isReservoir . snd) $ nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,(h,t,_)) -> h == i )
        correct i= suma . filter (\(_,(h,t,_)) -> t == i )
        suma =  fmap (\(li,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        nflow i e = genFlow (var i pm) e

genFlow _ (Open i ) = i
genFlow _ (Tee _ _ ) = 0
genFlow idf (Sprinkler (_,k) _ _ _) = if idf <= 0 then negate k*sqrt (abs idf) else k*sqrt(abs idf)
genFlow _ i = errorWithStackTrace $ "doesn't preserve flow: " <> show i


-- Generic Solver | Node + Head Method
jacobianNodeHeadEquation :: (Show a,Ord a,Floating a) => Ambient a -> Grid Element a -> M.Map Int a ->M.Map Int a -> [a]
jacobianNodeHeadEquation am grid  vm nh =  term <$> l
  where
    l = links grid
    sflow = signedFlow grid vm
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t conf ) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTee (fluido am) conf (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow grid
    addTee k = maybe 0 id (M.lookup k nodeLosses)
    term (l,(h,t,e)) =   sum (pipeElement am (var l vm) <$> e)  + gravityEffect am (var t nhs ^. _z - var h nhs ^. _z)  + (var t nh - var h nh )  +  addTee (h,l) + addTee (t,l)
      where
         nhs = fmap fst (M.fromList $shead grid)

gravityEffect am dh = (density (fluido am) - densidadeAmbiente am)/(density (fluido am))*dh*(gravidade am)

-- Multiple fluid equations
darcy :: (Ord a,Show a,Floating a) => Element a -> Ambient a -> a -> a
darcy e flu q = f*l/d*ve^2/(2*g)
  where
    l = distanciaE e
    d =  diametroE e
    g = gravidade flu
    vis = kinematicViscosity (fluido flu)
    re = ve*d/vis
    qm3s = q/1000/60
    ve = qm3s/areaE e
    f
      | re < 4000  = 64 /re
      | re  <= 10^8 =  1 + (20000*roughnessE e/d + 10.0e6/re)
      | otherwise = errorWithStackTrace $ "no equation for this condition" <> show (qm3s,ve,vis,re,roughnessE e,d)

-- Open tube equation
manning t  = perda*10/(1000*60)**1.85
        where
              d = diametroE t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 4.66*(distanciaE t)*c/(d**(16/3))


-- Darcy water equations
ktubo t  = perda*10/(1000*60)**1.85
  where
    d = diametroE t
    c = materialE t
    -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
    perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))

jacobianEqNodeHeadGrid :: (Show a , Ord a ,Floating a) => Ambient a -> Grid Element a -> M.Map Int (LinkDomain Element a) -> M.Map Int (LinkDomain Element  a) -> [a]
jacobianEqNodeHeadGrid e = (\l v h -> jacobianNodeHeadEquation e l (runIdentity <$> v) (runIdentity <$> h) <> jacobianContinuity l (runIdentity <$> v) (runIdentity <$> h))
{-
rel = [tubod dm 0.5 , Turn (1/4) ,joelhoR  , tubod dm 1.2 , joelhoL,Turn (-1/4) , tubod dm 0.5]
  where
      dm = 0.08
      tubod  d l  = Tubo (Just d) l 100
      joelhoR  = Joelho right90 $ TabelaPerda Nothing ("Conexao","Joelho","90") 100
      joelho  = joelhoR
      joelhoL  = Joelho left90 $ TabelaPerda  Nothing ("Conexao","Joelho","90") 100
      rightC d = d
      right90  = rightC $ 1/4
      leftC d = -d
      left90  = leftC $ 1/4

rori = (V3 1 0 (0::Double) , SO3 $rotM (V3 0 0 (-pi/2) :: V3 Double))

-}
