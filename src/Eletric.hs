{-# LANGUAGE FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
module Eletric where

import Data.Monoid
import Data.Functor.Identity
import Control.Monad
import Control.Applicative
import Data.Functor.Compose
import Domains
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as F


data Eletric a
  = Resistor
  { res :: a
  }
  | VoltageSource
  { ddp :: a
  }
  | Node
  | Ground
  deriving(Show,Eq,Ord,Functor)



instance PreSys Eletric where
  type NodeDomain Eletric = Identity
  type LinkDomain Eletric = Identity
  constrained (Ground ) = Identity (Just 0)
  constrained i = Identity $ Nothing
  lconstrained i = Identity $ Nothing





-- Generic Solver | Node + Head Method
circuitPotential :: (Show a,Ord a,Floating a) => Grid Eletric a -> M.Map Int a ->M.Map Int a -> [a]
circuitPotential grid  vm nh =  term <$> l
  where
    l = links grid
    term (l,h,t,e) =   sum (circuitElement (var l vm) <$> e) - lookNode h   +  lookNode t
    lookNode h = justError "cant find node " $ (join $ fmap (\i -> if isGround i then (\(Ground ) -> Just 0) $ i else Nothing) $ varM h (M.fromList $ nodesFlow grid)) <|> varM h nh
      where
        varM h = M.lookup h


isGround Ground = True
isGround i = False

circuitContinuity :: (Show a,Ord a,Floating a )=> Grid Eletric a -> M.Map Int a -> M.Map Int a -> [a]
circuitContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $ filter (not . isGround . snd) $ nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,h,t,_) -> h == i )
        correct i= suma . filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        genFlow _ Node  = 0
        genFlow _ Ground = 0
        nflow i e = genFlow (var i pm) e

circuitEq l vh = loops <> nodes
    where loops =  circuitPotential l v h
          nodes =  circuitContinuity l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh


circuitElement v (Resistor i ) = v*i
circuitElement _ (VoltageSource i ) = i
