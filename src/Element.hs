{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Element where

import Backend.DXF
import Backend.Mecha as Mecha
import Control.Applicative
import Control.Arrow
import Control.Lens ((^.), _1)
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import Debug.Trace
import Domains
import GHC.Stack
import Hydraulic
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import qualified Numeric.AD as AD
import Numeric.GSL.Root
import qualified Position as P
import Position
import Rotation.SO3 hiding (rotM)
import TBL.Parser
import Tee hiding (ktubo)

data Region = Region
  { regionName :: String,
    regionFile :: String,
    regionView :: [(String, String)],
    regionBoundary :: ([V2 Double], Double),
    regionRisk :: RegionRisk
  }

data RiskClass
  = Light
  | OrdinaryI
  | OrdinaryII
  | ExtraOrdinaryI
  | ExtraOrdinaryII
  deriving (Eq, Show, Ord)

data ComodityClass
  = ComodityI
  | ComodityII
  | ComodityIII
  deriving (Eq, Show, Ord)

data RegionRisk
  = Standard
      { stdrisk :: RiskClass
      }
  | MiscelaneousStorage
      { stdrisk :: RiskClass,
        storageHeight :: Double
      }
  | HighpilledStorage
      { commodityClass :: ComodityClass,
        storageHeight :: Double
      }
  deriving (Eq, Show, Ord)

water = Fluido [((20, 0.325), 1e-3), ((25, 101.325), 0.91e-3), ((25, 0.325), 0.91e-3), ((20, 101.325), 1e-3)] [((25, 0.325), 997), ((25, 101.325), 997)] Water

air = Fluido [((20, 101.325), 1.8e-5), ((50, 101.325), 1.95e-5), ((0, 101.325), 1.71e-5)] [((25, 101.325), air700mDensity)] Air

fluidDensity p t _ (Fluido _ d Water) = 997 -- justError ("no bilinear of density: " ++ show (p,t,d) )$ bilinearInterp (t,p) (M.fromList d)
fluidDensity p t amb (Fluido _ d Air) = airDensity (pontoOrvalho amb) t (1000 * p) (altitude amb)

fluidViscosity _ t (Fluido _ _ Water) = 0.00091
fluidViscosity p t f = justError ("no biliinterpolation of viscosity: " <> show (p, t, f)) $ bilinearInterp (t, p) (M.fromList $ viscosity f)

kinematicViscosity env p t f@(Fluido _ d Air) = airViscosity t / fluidDensity p t env f
kinematicViscosity env p t f = fluidViscosity p t f / fluidDensity p t env f

airDensity2 pb tdb dw = pb / (287.0 * (tdb + 273.15) * (1.0 + 1.6077687 * max dw 1.0e-5))

airViscosity tz = 1.71432e-5 + 4.828e-8 * tz

air700mDensity = 0.9

-- Pressure kpa ->  Temperature ºC ->  Density
idealGasDensity :: Fractional a => a -> a -> a
idealGasDensity p t = p * 1000 / rAirConstant / tk
  where
    tk = t + 273.15

defAmbient f a =
  Ambient f a 700 25 15 1103 (V2 (16.605621) (-49.3481687))

instance PreSys Element where
  -- Global Variables
  type Enviroment Element = Ambient

  -- Node Variables
  type NodeDomain Element = V3

  -- Link Variables
  type LinkDomain Element = Identity
  initIter g = (\e -> Iteration (fmap Compose <$> varsL) (fmap Compose <$> varsN) e g)
    where
      varsN = fst $ runState (((traverse (traverse (traverse conv . constrained)))) $ nodes g) 50
      conv (Just i) = return Nothing
      conv Nothing = do
        i <- get
        put 50
        return (Just i)
      convL (Just i) = Nothing
      convL Nothing = (Just 98)
      varsL = fmap (fmap ((fmap convL . lconstrained))) $ (fmap (\(i, (_, _, l)) -> (i, l)) $ links g)
  constrained (Tee (TeeConfig _ _ (DuctEntry _ _)) _) = V3 Nothing Nothing Nothing
  constrained (Tee (TeeConfig _ _ (RectangularEntry _ _ _)) _) = V3 Nothing Nothing Nothing
  constrained (Tee (TeeConfig _ _ (RoundEntry _ _)) _) = V3 Nothing Nothing Nothing
  constrained (Reservatorio i) = V3 Nothing Nothing Nothing
  constrained (Grelha _ _ _ _) = V3 Nothing Nothing Nothing
  constrained (Open i) = V3 Nothing (Just i) Nothing
  constrained (Sprinkler (_, k) _ _ _) = V3 Nothing Nothing Nothing
  constrained i = V3 Nothing 0 Nothing
  lconstrained i = Identity $ Nothing

idealGasLaw p t d = p - t * rGasConstant * d

-- Earth Gravity Equations
gravitySea phi = 9.7803253359 * (1 + 0.00193185265241 * (sin phi) ^ 2 / (sqrt (1 -0.00669437999013 * (sin phi) ^ 2)))

gravityHeight lat h = gravitySea lat - 3.155e-7 * h

localGravity am = gravityHeight (geoposition am ^. _x) (altitude am)

-- Gravity Field Pressure Correction
airActualPressure as h = p
  where
    k1 = 0.190263
    k2 = 8.417286E-5
    p = ((as ** k1) - (k2 * geoPotential h)) ** (1 / k1)

geoPotential z = ((r * z) / (r + z))
  where
    r = 6369E3

rGasConstant, airMolarApproximate, rAirConstant :: Fractional a => a
rGasConstant = 8.31447
airMolarApproximate = 2.8964425307777524e-2 --- kg/mol
rAirConstant = rGasConstant / airMolarApproximate -- 287.058

airPressure h = p0 * (1 - g * h / cp / t0) ** (cp * mair / rGasConstant)
  where
    t0 = 288.15
    p0 = 101.325
    cp = 1007
    g = 9.80665
    mair = 0.0289644

airDensity :: Floating a => a -> a -> a -> a -> a
airDensity tdew temp pressure height = rho * 100
  where
    rv = 461.4964
    rd = 287.0531
    tk = temp + 273.15
    rho = pd / (rd * tk) + pv / (rv * tk)
    es t = eso / (p t) ^ 8
    eso = 6.1078
    p t = c0 + t * (c1 + t * (c2 + t * (c3 + t * (c4 + t * (c5 + t * (c6 + t * (c7 + t * (c8 + t * (c9)))))))))
    pv = es tdew
    pd = airActualPressure pressure height - pv
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
  thisElement [a, b] (Bomba _ _) = (2,) <$> M.fromList [(a, (0, so3 0)), (b, (V3 0 0 0, so3 (V3 0 0 pi)))]
  thisElement [a, b] (Perda _) = (2,) <$> M.fromList [(a, (0, so3 0)), (b, (V3 0 0 0, so3 (V3 0 0 pi)))]
  thisElement [a, b] (Tubo _ c _) = (2,) <$> M.fromList [(a, (0, so3 0)), (b, (V3 (c) 0 0, so3 (V3 0 0 pi)))]
  thisElement [a, b] (Joelho c _) = (2,) <$> M.fromList [(a, (0, so3 0)), (b, (0, so3 (V3 0 0 (pi + opi c))))]
  thisElement [a, b] (Turn c) = (0,) <$> M.fromList [(a, (0, so3 0)), (b, (0, so3 (V3 (pi + opi c) 0 0)))]
  thisElement _ (Tee (TeeConfig [rl, rr] _ (FanSystemInteraction (Elbow ang _ _) len _)) _) = (2,) . (\(l, j) -> (V3 0 (- l) 0, so3 $ fmap opi (V3 0 0 j))) <$> M.fromList [(rl, (len, 0)), (rr, (0, 1 / 2 + ang / 360))]
  thisElement l i = (2,) . (\j -> (0, so3 $ fmap opi (V3 0 0 j))) <$> this (l, i)
    where
      this = M.fromList . els
        where
          els (_, (Tee (TeeConfig [rl, br, bl, rr] _ (RoundTee ang _ _)) _)) =
            [(rl, 1 / 2 - t), (rr, - t), (br, 0), (bl, 1 / 2)]
            where
              t = ang / pi / 2
          els (_, (Tee (TeeConfig [rl, b, rr] _ (RoundTee ang _ _)) _)) =
            [(rl, 1 / 2 - t), (rr, - t), (b, 0)]
            where
              t = ang / pi / 2
          els (_, (Tee (TeeConfig [rl, b, rr] _ (RectangularTee _)) _)) =
            [(rl, 1 / 2 - t), (rr, - t), (b, 0)]
            where
              t = 1 / 4
          els (_, (Tee (TeeConfig [rl, rr] _ (Elbow ang _ _)) _)) =
            [(rl, 0), (rr, 1 / 2 + ang / 360)]
            where
              t = 1 / 4
          els ([a, b], i) =
            [(a, 0), (b, 1 / 2)]
          els ([a], i) =
            [(a, 0)]
          els i = errorWithStackTrace $ show ("thisElement", i)

pipeElement am ps v e | v < 0 = negate $ pipeElement am ps (abs v) e
pipeElement am ps v (Bomba (pn, vn) (Poly l)) = res
  where
    polyTerm (0, c) = c
    polyTerm (p, c) = c * (100 * v / vn) ** p
    res = negate $ (* pn) $ (/ 100) $foldr1 (+) (polyTerm <$> l)
pipeElement am ps v e@(Tubo _ _ _) =
  case fluidName $ fluido am of
    Water -> ktubo am ps joelhos e v
    Air -> darcy ps e am v
pipeElement am ps v e@(Joelho _ _) = ktubo am ps joelhos e v
pipeElement am ps v e@(Perda _) = ktubo am ps joelhos e v
pipeElement am ps v (Turn _) = 0

signedFlow :: (Show a, Floating a) => Grid Element a -> M.Map Int a -> M.Map Int (M.Map Int a)
signedFlow g v = M.fromList $ fmap (\(i, _) -> (i,) $ M.fromList $ (sumn $ flipped i $ links g) ++ ((suma $ correct i $ links g))) (nodes g)
  where
    flipped i = filter (\(_, (h, t, _)) -> h == i)
    correct i = filter (\(_, (h, t, _)) -> t == i)
    suma = fmap (\(li, _) -> (li, var li v))
    sumn = fmap (\(li, _) -> (li, negate $ var li v))

jacobianContinuity :: (Show a, Ord a, Floating a) => Grid Element a -> M.Map Int a -> M.Map Int (V3 a) -> [a]
jacobianContinuity g v pm = fmap (\(i, e) -> sum (flipped i $ links g) + (sum (correct i $ links g)) - nflow i e) $ nodes g
  where
    -- pipeFlow
    flipped i = sumn . filter (\(_, (h, t, _)) -> h == i)
    correct i = suma . filter (\(_, (h, t, _)) -> t == i)
    suma = fmap (\(li, _) -> var li v)
    sumn = fmap negate . suma
    -- nodeFlow
    nflow i e = var i pm ^. _y

zoneEquations am g pm = catMaybes $ uncurry nflow <$> nodes g
  where
    nflow i e = do
      fl <- genFlow (var i (M.fromList (nodesPosition g)) ^. _1 . _z) e
      return $ var i pm ^. _x - fl
    genFlow idf (Grelha _ _ _ _) = Just $ airPressure (idf + altitude am)
    genFlow idf (Tee (TeeConfig _ _ (RoundEntry _ _)) _) = Just $ airPressure (idf + altitude am)
    genFlow idf (Tee (TeeConfig _ _ (DuctEntry _ _)) _) = Just $ airPressure (idf + altitude am)
    genFlow idf (Tee (TeeConfig _ _ (RectangularEntry _ _ _)) _) = Just $ airPressure (idf + altitude am)
    genFlow idf (Reservatorio _) = Just $ airPressure (idf + altitude am)
    genFlow _ i = Nothing

thermalEquations am g pm = catMaybes $ uncurry nflow <$> nodes g
  where
    nflow i e = do
      fl <- genFlow (var i (M.fromList (nodesPosition g)) ^. _1 . _z) (var i pm ^. _x) e
      return $ var i pm ^. _z - fl
    genFlow _ _ i = Just $ temperaturaAmbiente am

leakEquations am g pm = catMaybes $ uncurry nflow <$> nodes g
  where
    nflow i e = do
      fl <- genFlow (var i (M.fromList (nodesPosition g)) ^. _1 . _z) (var i pm) e
      return $ var i pm ^. _y - fl
    genFlow h (V3 p v t) (Sprinkler (_, k) _ _ _) = Just $ (if p <= 0 then negate else id) flow
      where
        flow = k * sqrt (abs pdrop)
        pdrop = p - airPressure (h + altitude am)
    genFlow h (V3 p v t) (PowerLawFlow expn coef)
      | laminar && p >= 0 = Just $ coef * rho / visc * (rhoNorm / rho) ** (expn -1) * (viscNorm / visc) ** (2 * expn -1) * pdrop
      | laminar && p < 0 = Just $ coef * rho2 / visc2 * (rhoNorm / rho2) ** (expn -1) * (viscNorm / visc2) ** (2 * expn -1) * pdrop
      | p >= 0 = Just $ coef * sqrt rho * (abs pdrop) ** expn
      | p < 0 = Just $ - coef * sqrt rho2 * (abs pdrop) ** expn
      where
        viscNorm = fluidViscosity 101.325 20 (fluido am)
        rhoNorm = fluidDensity 101.325 20 am (fluido am)
        rho = fluidDensity p t am (fluido am)
        rho2 = fluidDensity (airPressure (h + altitude am)) (temperaturaAmbiente am) am (fluido am)
        visc = fluidViscosity p t (fluido am)
        visc2 = fluidViscosity (airPressure (h + altitude am)) (temperaturaAmbiente am) (fluido am)
        laminar = False
        pdrop = p - airPressure (h + altitude am)
    genFlow _ _ i = Nothing

fittingsCoefficient am nh sflow n t = case fluidName (fluido am) of
  Water -> (\(ix, l) -> (ix, ktubo am (nstate, nstate) joelhos l (abs $ var ix flowMap))) <$> fittingLossesNFPA (fluido am) joelhos flowMap t
  Air -> pressureDrop (nstate ^. _x) (nstate ^. _z) (fluido am) flowMap (sectionMap t) <$> fittingLosses (fluido am) (ashrae am) flowMap t
  where
    nstate = var n nh
    flowMap = (fmap (\x -> x / 1000 / 60) $ var n sflow)

-- Generic Solver | Node + Head Method
jacobianNodeHeadEquation :: (Show a, Ord a, Floating a, Real a) => Ambient a -> Grid Element a -> M.Map Int a -> M.Map Int (V3 a) -> [a]
jacobianNodeHeadEquation am grid vm nh = term <$> l
  where
    l = links grid
    sflow = signedFlow grid vm
    fittings n t = fittingsCoefficient am nh sflow n t
    nodeLosses = M.fromList . concat . fmap (\(n, Tee t conf) -> (\(ti, v) -> ((n, ti), v)) <$> fittings n t) . filter (isTee . snd) $ nodes grid
    addTee k = maybe 0 id (M.lookup k nodeLosses)
    term (l, (h, t, e)) = sum (pipeElement am (var h nh, var t nh) (var l vm) <$> e) + stackEffect (var h nh, var t nh) am (var t nhs ^. _z) (var h nhs ^. _z) (var l vm) + (var t nh ^. _x - var h nh ^. _x) + addTee (h, l) + addTee (t, l)
      where
        nhs = fmap fst (M.fromList $ nodesPosition grid)

testStack i am h1 h2 f = (gravityEffect i am (h2 - h1), stackEffect i am h1 h2 f)

gravityEffect (V3 p _ t, _) am dh = fluidDensity p t am (fluido am) * dh * (localGravity am) / 1000

stackEffect (V3 p1 _ t1, V3 p2 _ t2) am h1 h2 f
  | f > 0 = localGravity am * (fluidDensity p1 t1 am (fluido am) * dh + h2 * drho) / 1000
  | f < 0 = localGravity am * (fluidDensity p2 t2 am (fluido am) * dh + h1 * drho) / 1000
  | otherwise = localGravity am * ((fluidDensity p2 t2 am (fluido am) + fluidDensity p2 t2 am (fluido am)) * dh + (h1 + h2) * drho) / 2 / 1000
  where
    dh = h2 - h1
    drho = fluidDensity p2 t2 am (fluido am) - fluidDensity p1 t1 am (fluido am)

-- System Equations
reinolds env p t v d f = v * d / kinematicViscosity env p t f

-- Multiple fluid equations , input Flow;L/min , output Pressure;kPA

darcy :: (Ord a, Show a, Floating a, Real a) => (V3 a, V3 a) -> Element a -> Ambient a -> a -> a
darcy (V3 p _ t, _) e flu q = f * l / d * ve ^ 2 / 2 * fluidDensity p t flu (fluido flu) / 1000
  where
    l = distanciaE e
    d = diametroE e
    g = localGravity flu
    re = reinolds flu p t ve d (fluido flu)
    qm3s = q / 1000 / 60
    ve = qm3s / areaE e
    f
      | re < 4000 = 64 / re
      | 4000 < re && re <= 10 ^ 8 = colebrook re d (roughnessE e)
      | otherwise = errorWithStackTrace $ "no equation for this condition" <> show (qm3s, ve, re, roughnessE e, d)

-- Aproximate Colebrook Equation
colebrookAprox re dh rough = (6.4 / (log re - log (1 + 0.01 * re * rough / dh * (1 + 10 * sqrt (rough / dh)))) ** 2.4)

-- Root of colebrook equation
colebrook :: (Floating a, Real a) => a -> a -> a -> a
colebrook re dh rough = realToFrac $ fst $ uniRootJ Steffenson 1e-8 100 equation (AD.diff equation) (realToFrac $ colebrookAprox re dh rough)
  where
    equation :: forall a. Floating a => a -> a
    equation = (\k -> 1 / sqrt k + 2 * logBase 10 (2.51 / (realToFrac re * sqrt k) + (realToFrac rough / realToFrac dh) / 3.72))

-- Open tube Manning equation
manning t = perda * 10 / (1000 * 60) ** 1.85
  where
    d = diametroE t
    c = materialE t
    -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
    perda = 4.66 * (distanciaE t) * c / (d ** (16 / 3))

-- Hazen Willians water equations , input (flow;L/min) ; output (Pressure;KPA )
ktubo env (V3 p _ tk, _) joelhos t q = perda / 1000 * fluidDensity p tk env (fluido env) * localGravity env * (q / 1000 / 60) ** 1.85
  where
    d = diametroE t
    c = materialE t
    -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
    perda = 10.67 * (distanciaE t) / (c ** 1.85) / (d ** 4.87)

jacobianEqNodeHeadGrid :: (Show a, Ord a, Floating a, Real a) => Ambient a -> Grid Element a -> M.Map Int (LinkDomain Element a) -> M.Map Int (NodeDomain Element a) -> [a]
jacobianEqNodeHeadGrid e l v h = jacobianNodeHeadEquation e l (runIdentity <$> v) h <> jacobianContinuity l (runIdentity <$> v) h <> leakEquations e l h <> zoneEquations e l h <> thermalEquations e l h

--------------------------
-- Mecha Backend        --
--------------------------

renderElemMecha ni (Open i) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere 0.1) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Reservatorio i) = (Mecha.color (1, 1, 1, 1) $ Mecha.sphere 0.5) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Grelha _ _ _ _) = (Mecha.color (1, 1, 1, 1) $ Mecha.sphere 0.5) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is, os] (Elbow _ _ _)) _) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is, os] (Damper _ _)) _) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is, os] (FanSystemInteraction j@(Elbow ang _ _) len t)) _) = Mecha.color (1, 1, 0, 1) $ (Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni))) <> Mecha.rotateZ (pi * ang / 180) (renderLinkMecha undefined ni (Tubo os len 100))
renderElemMecha ni (Tee (TeeConfig _ [is, os] (FireDamper)) _) = (Mecha.color (1, 0, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is, os] (Transition _)) _) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is, os] (Screen _ _)) _) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is] (RectangularEntry _ _ _)) _) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is] (RoundEntry _ _)) _) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [is] (DuctEntry _ _)) _) = (Mecha.color (0, 1, 0, 1) $ Mecha.sphere (hydraulicDiameter is * 1.05)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [Circular i, Circular j, _] _) _) = (Mecha.color (1, 0, 0, 1) $ Mecha.rotateY (- pi / 2) $ Mecha.moveZ (-0.5 * j) $ Mecha.cone i (2 * j) (2 * j)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Tee (TeeConfig _ [r, Rectangular i j, _] _) _) = (Mecha.color (1, 0, 0, 1) $ Mecha.rotateY (- pi / 2) $ Mecha.moveZ (-0.5 * j) $ Mecha.cone i (2 * j) (2 * j)) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni (Sprinkler ((d, k)) _ fa@(SPKCoverage sx sy sz (SPKGoods g _)) a) = (Mecha.sphere 0.15) <> (Mecha.moveY 0.2 $ Mecha.scale (0.03, 0.03, 0.03) (Mecha.text (show ni)))
renderElemMecha ni i = errorWithStackTrace $ show ("renderElemMecha", ni, i)

renderLinkMecha _ ni (Tubo s c _) =
  ( case s of
      Circular d -> (Mecha.color (0.2, 0.2, 1, 1) $ Mecha.rotateY (pi / 2) $ Mecha.cylinder d (c * 0.9999))
      Rectangular h w -> Mecha.color (0.2, 0.2, 1, 1) $ Mecha.scale (c, w, h) $ Mecha.move (0.5, 0, 0) $ Mecha.cube 1
  )
renderLinkMecha nis ni (Joelho _ (TabelaPerda (d) c _)) = Mecha.sphere (hydraulicDiameter d) -- <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha nis ni (Bomba i v) = Mecha.moveX (0.03 / 2) $ Mecha.sphere 0.4 -- <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha nis _ o = Mecha.sphere 0.02

instance Target Element Mecha.Solid where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha

drawRegion (Region n _ _ (i, h) _) = moveZ h (color (1, 0, 0, 0.1) $extrude (Mecha.polygon ((\(V2 i j) -> [i, j]) <$> i) [[0 .. (length i)]]) 1) <> tpos (text n)
  where
    tpos = move (head i ^. _x, head i ^. _y, h) . scale (0.09, 0.09, 0.09)

----
-- DXF Backend
---

instance Target Element [EntityTy] where
  renderLink nis ni (Tubo (Circular d) c _) = [TEXT (V3 (c / 2) 0.3 0) 0.2 (show $ round (d * 1000)) Nothing Nothing, LINE 0 (V3 c 0 0)]
  renderLink nis ni (Joelho _ (TabelaPerda (d) c _)) = []
  renderLink nis ni i = [CIRCLE 0 0.2]
  renderNode nis (Sprinkler _ _ _ _) = [INSERT "spk" 0 (Just 1) Nothing Nothing []]
  renderNode nis (Tee _ _) = []
  renderNode nis (Open _) = []
  renderNode nis i = [CIRCLE 0 0.2]
