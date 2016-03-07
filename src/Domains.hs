{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts,DeriveFunctor,TypeFamilies #-}
module Domains
  (Grid(..)
  ,PreSys (..)
  ,var
  ,Iteration
  ,FIteration (..)
  ,runState
  ,uarse
  ,uarseTup
  ,uarseT
  ,parse
  ,parseTup
  ,parseT
  ,prepareModel
  ,nodesSet
  ,justError
  ,PreCoord(..)
  ,Coord(..)
  )where

import Linear.V3
import Data.Set(Set)
import Data.Functor.Compose
import Data.Monoid
import Rotation.SO3
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import GHC.Stack
import System.IO.Unsafe

data Grid b a
  = Grid
  { linksPosition :: [(Int,[(V3 a ,SO3 a)])]
  , links :: [(Int,(Int,Int,[b a]))]
  , surfaces :: [(Int,([Int],b a))]
  , shead :: [(Int,(V3 a,SO3 a))]
  , nodesFlow :: [(Int,b a)]
  }deriving(Functor,Show)


class PreSys sys  where
  type NodeDomain sys  :: (* -> *)
  type LinkDomain sys  :: (* -> *)
  revElem :: Num a => sys  a -> sys a
  initIter :: (Functor (LinkDomain sys),Traversable (NodeDomain sys),Fractional a) => Grid sys a -> Iteration  sys a
  initIter g = Iteration  (fmap Compose <$> varsL) (fmap Compose <$> varsN) g
    where
      varsN = fst  $ runState (((traverse (traverse (traverse conv  . constrained )))) $ nodesFlow g) 1
      conv (Just i) = return Nothing
      conv Nothing = do
        i <- get
        put (i+0.1)
        return (Just  i)
      convL (Just i) = Nothing
      convL Nothing = (Just 2)
      varsL = fmap (fmap ((fmap convL . lconstrained ))) $ (fmap (\(i,(_,_,l))-> (i,l)) $  links g)
  constrained :: Num a => sys a -> NodeDomain sys (Maybe a)
  lconstrained :: Num a => [sys a]-> LinkDomain sys (Maybe a)

type Iteration sys  a =  FIteration (NodeDomain sys ) (LinkDomain sys) sys a
data FIteration n l  b a
  = Iteration
  { flows :: [(Int,Compose l Maybe a)]
  , pressures :: [(Int,Compose n Maybe a)]
  , grid :: Grid b a
  }deriving(Show,Functor)

class PreCoord a  where
  type Ang a
  trans :: (a,Ang a) -> (a,Ang a) -> (a,Ang a)
  untrans :: (a,Ang a) -> (a,Ang a) -> (a,Ang a)
  dist :: (a,Ang a) -> (a,Ang a) -> (Double,Double)

class (PreSys sys ,PreCoord a) => Coord sys a where
  nextElement :: Int -> (Set Int,(Int,sys Double)) -> [(Int,(a,Ang a))]
  thisElement :: (Set Int,(Int,sys Double)) -> M.Map Int (a,Ang a)
  nodeTrans :: sys Double -> [(Int,(a,Ang a))]
  elemTrans :: sys Double -> (a,Ang a)



var :: Show a => Int -> M.Map Int a -> a
var i m = case M.lookup i m of
               Just i -> i
               Nothing -> errorWithStackTrace $ "no variable " ++ show i  ++ " " ++ show m

uarse :: Maybe a -> State [a] (Maybe a)
uarse  (Just i) = do
  return $ Nothing
uarse Nothing = do
  i:l <- get
  put l
  return $ Just  i

uarseTup (x,v) = do
  (,) <$> uarseT  x <*>  uarseT  v

uarseT :: Traversable f => f (Maybe a) -> State [a] (f (Maybe a))
uarseT  v =  traverse uarse  v


parse  (Just i) = do
  return i
parse Nothing = do
  v <- get
  case v of
    i:l -> do
      put l
      return i
    l -> errorWithStackTrace ("parseError" <>  show l)

parseTup (x,v) = do
  (,) <$> parseT  x <*>  parseT v

parseT  v = do
  traverse parse  v


justError e Nothing = error ("justError" <> e)
justError _ (Just i) = i

prepareModel l model vh = model l v h
    where
      v = M.fromList linksIn
      h = M.fromList nodesIn
      (nodesIn,linksIn) = fst $ runState ((,) <$> nodesInP <*> linksInP ) (vh  <> replicate 10 100)
      nodesInP = traverse (traverse (traverse parse .constrained)) (nodesFlow l)
      linksInP = traverse (traverse (traverse parse .lconstrained)) (fmap (\(i,(_,_,j)) -> (i,j)) $ links l)

nodesSet grid = fmap (\(i,n) -> (i,(var i nodeMapSet,n))) (nodesFlow grid)
    where nodeMapSet = fmap S.fromList $ M.fromListWith mappend $ concat $ (\(l,(h,t,_)) -> [(h,[l ]),(t,[l ])]) <$> links grid
