{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Domains
  ( Grid (..),
    PreSys (..),
    var,
    varM,
    Iteration,
    FIteration (..),
    runState,
    nodesSet,
    justError,
    PreCoord (..),
    Coord (..),
    RBackend (..),
    Target (..),
  )
where

import Control.Applicative
import Control.Monad.State
import Data.Functor.Compose
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import GHC.Stack
import Linear.V3
import Rotation.SO3

data Grid b a = Grid
  { linksPosition :: [(Int, [(V3 a, SO3 a)])],
    links :: [(Int, (Int, Int, [b a]))],
    surfaces :: [(Int, ([(Bool, Int)], b a))],
    volumes :: [(Int, ([(Bool, Int)], b a))],
    nodesPosition :: [(Int, (V3 a, SO3 a))],
    nodes :: [(Int, b a)],
    enviroment :: [(Int, b a)],
    enviromentPosition :: [(Int, (V3 a, SO3 a))]
  }
  deriving (Functor, Show)

class PreSys sys where
  type Enviroment sys :: (* -> *)
  type NodeDomain sys :: (* -> *)
  type LinkDomain sys :: (* -> *)
  type SurfaceDomain sys :: (* -> *)
  initIter :: (Functor (LinkDomain sys), Traversable (NodeDomain sys), Fractional a) => Grid sys a -> (Enviroment sys a -> Iteration sys a)
  initIter g = (\e -> Iteration (fmap Compose <$> varsL) (fmap Compose <$> varsN) e g)
    where
      varsN = fst $ runState (((traverse (traverse (traverse conv . constrained)))) $ nodes g) 1
      conv (Just i) = return Nothing
      conv Nothing = do
        i <- get
        put (100 + i * 0.1)
        return (Just i)
      convL (Just i) = Nothing
      convL Nothing = (Just 2)
      varsL = fmap (fmap ((fmap convL . lconstrained))) $ (fmap (\(i, (_, _, l)) -> (i, l)) $ links g)
  constrained :: Num a => sys a -> NodeDomain sys (Maybe a)
  lconstrained :: Num a => [sys a] -> LinkDomain sys (Maybe a)
  postprocess :: (Show a, Floating a) => Iteration sys a -> [(Int, SurfaceDomain sys a)]
  postprocess _ = []

type Iteration sys a = FIteration (NodeDomain sys) (LinkDomain sys) (Enviroment sys) sys a

data FIteration n l o b a = Iteration
  { flows :: [(Int, Compose l Maybe a)],
    pressures :: [(Int, Compose n Maybe a)],
    environment :: o a,
    grid :: Grid b a
  }
  deriving (Show, Functor)

class PreCoord a where
  type Ang a :: * -> *
  trans :: RealFloat b => (a b, Ang a b) -> (a b, Ang a b) -> (a b, Ang a b)
  untrans :: RealFloat b => (a b, Ang a b) -> (a b, Ang a b) -> (a b, Ang a b)
  dist :: RealFloat b => (a b, Ang a b) -> (a b, Ang a b) -> (b, b)

class (PreSys sys, PreCoord a) => Coord sys a where
  thisElement :: RealFloat b => [Int] -> sys b -> M.Map Int (Int, (a b, Ang a b))

varM :: Ord k => k -> Map k a -> Maybe a
varM i j = case M.lookup i j of
  Nothing -> Nothing
  i -> i

var :: (Ord b, Show b, Show a) => b -> M.Map b a -> a
var i m = case M.lookup i m of
  Just i -> i
  Nothing -> error $ "no variable " ++ show i ++ " " ++ show m

justError e Nothing = error ("justError" <> e)
justError _ (Just i) = i

nodesSet grid = fmap (\(i, n) -> (i, (var i nodeMapSet, n))) (nodes grid)
  where
    nodeMapSet = M.fromListWith mappend $ concat $ (\(l, (h, t, _)) -> [(h, [l]), (t, [l])]) <$> links grid

instance Num a => Num (Maybe a) where
  fromInteger i = Just (fromInteger i)
  i + j = liftA2 (+) i j
  negate = fmap negate
  abs = fmap abs
  i * j = liftA2 (*) i j
  signum = fmap signum

instance Fractional a => Fractional (Maybe a) where
  fromRational i = Just (fromRational i)
  recip = fmap recip
  i / j = liftA2 (/) i j

class RBackend a where
  type TCoord a :: * -> *
  type TField a :: *
  transformElement :: (TCoord a (TField a), Ang (TCoord a) (TField a)) -> a -> a
  errorItem :: a
  statements :: [a] -> a

class RBackend a => Target sys a where
  renderNode :: Int -> sys Double -> a
  renderLink :: Int -> Int -> sys Double -> a
  renderSurface :: [(Bool, (Int, Int, [sys Double]))] -> [(Int, (V3 Double, SO3 Double))] -> sys Double -> a
  renderVolume :: [[(Bool, (Int, Int, [sys Double]))]] -> [(Int, (V3 Double, SO3 Double))] -> sys Double -> a
  renderNodeSolve :: NodeDomain sys Double -> Int -> sys Double -> a
  renderLinkSolve :: LinkDomain sys Double -> sys Double -> a
  renderSurfaceSolve :: [(Int, SurfaceDomain sys Double)] -> [(Bool, (Int, Int, [sys Double]))] -> [(Int, (V3 Double, SO3 Double))] -> sys Double -> a -> a
