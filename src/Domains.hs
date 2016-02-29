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
  ,justError
  ,PreCoord(..)
  ,Coord(..)
  )where

import Linear.V3
import Data.Set(Set)
import Data.Functor.Compose
import Data.Monoid
import Rotation.SO3
import qualified Data.Map as M
import Control.Monad.State
import GHC.Stack

data Grid b a
  = Grid
  { linksPosition :: [(Int,[(V3 a ,SO3 a)])]
  , links :: [(Int,Int,Int,[b a])]
  , shead :: [(Int,(V3 a,SO3 a))]
  , nodesFlow :: [(Int,b a)]
  }deriving(Functor,Show)


class PreSys sys  where
  type NodeDomain sys  :: (* -> *)
  type LinkDomain sys  :: (* -> *)
  revElem :: Num a => sys  a -> sys a
  initIter :: (Functor (LinkDomain sys),Functor (NodeDomain sys),Num a) => Grid sys a -> Iteration  sys a
  initIter g = Iteration  (fmap Compose <$> varsL) (fmap Compose <$> varsN) g
    where
      varsN = fmap (fmap (fmap conv . constrained )) $ nodesFlow g
      conv (Just i) = Nothing
      conv Nothing = (Just 100)
      convL (Just i) = Nothing
      convL Nothing = (Just 400)
      varsL = fmap (fmap ((fmap convL . lconstrained ))) $ (fmap (\(i,_,_,l)-> (i,l)) $  links g)
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
  dist :: a -> a -> Double

class (PreSys sys ,PreCoord a) => Coord sys a where
  nextElement :: Int -> (Set Int,(Int,sys Double)) -> [(Int,(a,Ang a))]
  thisElement :: Int -> (Set Int,(Int,sys Double)) -> (a,Ang a)
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
