{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Grid where

import Control.Monad.State
import qualified Data.Foldable as F
import Data.Functor.Compose
import Debug.Trace
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Domains
import GHC.Stack
import Numeric.AD
import Numeric.GSL.Root

printJacobian ::
  forall p t m c.
  (Functor t, Show m, Ord m, Floating m, RealFloat m, Traversable (NodeDomain c), Traversable (LinkDomain c), PreSys c) =>
  (forall a. (Show a, Ord a, Floating a, RealFloat a) => Iteration p c a) ->
  (forall p b. (Show b, Ord b, Floating b, RealFloat b) => CoordinateGrid p b -> Grid c b -> M.Map Int (LinkDomain c b) -> M.Map Int (NodeDomain c b) -> t b) ->
  t [m]
printJacobian iter@(Iteration fl f e a _ ) modeler = jacobian (prepareModel (position iter) (grid iter) modeler) $ (inNodes <> inLinks)
  where
    inNodes = (concat $ (catMaybes . F.toList . getCompose) . snd <$> f)
    inLinks = (concat $ (catMaybes . F.toList . getCompose) . snd <$> fl)

printResidual ::
  forall p b t1 t2 t3 t sys. ( Num b,
    Show b,
    Foldable t1,
    Foldable t2,
    Traversable (LinkDomain sys),
    Traversable (NodeDomain sys),
    PreSys sys
  ) =>
  FIteration p t2 t1 t sys b ->
  ( CoordinateGrid  p b -> Grid sys b ->
    M.Map Int (LinkDomain sys b) ->
    M.Map Int (NodeDomain sys b) ->
    t3
  ) ->
  t3
printResidual iter@(Iteration fl f e a p ) modeler = (prepareModel p a modeler) (inNodes <> inLinks)
  where
    inNodes = concat $ (catMaybes . F.toList . getCompose) . snd <$> f
    inLinks = concat $ (catMaybes . F.toList . getCompose) . snd <$> fl

solveIter ::
  forall d c p.
  (Show d, Ord d, Floating d, RealFloat d, Traversable (NodeDomain c), Traversable (LinkDomain c), PreSys c) =>
  (forall a. (Show a, Ord a, Floating a, RealFloat a) => Iteration p c a) ->
  (forall b. (Show b, Ord b, Floating b, Real b, RealFloat b) => CoordinateGrid p b -> Grid c b -> M.Map Int (LinkDomain c b) -> M.Map Int (NodeDomain c b) -> [b]) ->
  Iteration p c d
solveIter iter@(Iteration fl f e g p) modeler = Iteration outLinks outNodes (environment iter) (grid iter) (position iter) 
  where
    (outNodes, outLinks) = (fst $ runState ((,) <$> nodesOutP g <*> linksOutP g) res)
    nodesOutP g = traverse (traverse (fmap (fmap realToFrac) . fmap Compose . uarseT . constrained)) (nodes g)
    linksOutP g = traverse (traverse (fmap (fmap realToFrac) . fmap Compose . traverse uarse . lconstrained)) (fmap (\(i, (_, _, p)) -> (i, p)) $ links g)
    inNodes = (concat $ (catMaybes . F.toList . getCompose) . snd <$> f)
    inLinks = (concat $ (catMaybes . F.toList . getCompose) . snd <$> fl)
    res = fst . rootJ HybridsJ 1e-3 1000 mod jmod $ inNodes <> inLinks
    mod = prepareModel (position iter) (grid iter) modeler
    jmod = jacobian (prepareModel (position iter) (grid iter) modeler)

-- Rendering System Equations
printMatrix :: Show a => [a] -> IO ()
printMatrix = putStr . unlines . fmap show

prepareModel2 ::
  ( Show b,
    PreSys sys,
    Num b,
    Traversable (LinkDomain sys),
    Traversable (NodeDomain sys)
  ) =>
  Grid sys b ->
  ( Grid sys b ->
    M.Map Int (LinkDomain sys b) ->
    M.Map Int (NodeDomain sys b) ->
    t
  ) ->
  [b] ->
  [b] ->
  t
prepareModel2  l model vl vn = model  l v h
  where
    v = M.fromList linksIn
    h = M.fromList nodesIn
    nodesIn = fst $ runState nodesInP vn
    linksIn = fst $ runState linksInP vl
    nodesInP = traverse (traverse (traverse parse . constrained)) (nodes l)
    linksInP = traverse (traverse (traverse parse . lconstrained)) (fmap (\(i, (_, _, j)) -> (i, j)) $ links l)


prepareModel ::
  ( Show b,
    PreSys sys,
    Num b,
    Traversable (LinkDomain sys),
    Traversable (NodeDomain sys))
  => CoordinateGrid p b 
  -> Grid sys b 
  -> (CoordinateGrid p b -> Grid sys b -> M.Map Int (LinkDomain sys b) -> M.Map Int (NodeDomain sys b) -> t ) 
  -> [b] -> t
prepareModel p l model vh = model p l v h
  where
    v = M.fromList linksIn
    h = M.fromList nodesIn
    (nodesIn, linksIn) = fst $ runState ((,) <$> nodesInP <*> linksInP) vh 
    nodesInP = traverse (traverse (traverse parse . constrained)) (nodes l)
    linksInP = traverse (traverse (traverse parse . lconstrained)) (fmap (\(i, (_, _, j)) -> (i, j)) $ links l)

uarse :: Maybe a -> State [a] (Maybe a)
uarse (Just i) = do
  return $ Nothing
uarse Nothing = do
  r <- get
  case r of
    (i : l) -> do
      put l
      return $ Just i
    [] -> return Nothing

uarseTup (x, v) = do
  (,) <$> uarseT x <*> uarseT v

uarseT :: Traversable f => f (Maybe a) -> State [a] (f (Maybe a))
uarseT v = traverse uarse v

parse (Just i) = do
  return i
parse Nothing = do
  v <- get
  case v of
    i : l -> do
      put l
      return i
    l -> errorWithStackTrace ("parseError" <> show l)

parseTup (x, v) = do
  (,) <$> parseT x <*> parseT v

parseT v = do
  traverse parse v
