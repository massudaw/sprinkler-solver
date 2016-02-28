{-# LANGUAGE FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
module Thermal where

import Data.Monoid
import Data.Functor.Identity
import Control.Monad
import Control.Applicative
import Data.Functor.Compose
import Domains
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as F

data Thermal a
  = HeatFlow
  { res :: a
  }
  | Conductor
  { conductance :: a
  }
  | Wall
  { material :: a
  , thickness :: a
  , sectionArea :: a
  }
  | ThermalNode
  | Ambient
  { ambient :: a
  }
  deriving(Show,Eq,Ord,Functor)

instance PreSys Thermal where
  type NodeDomain Thermal = Identity
  type LinkDomain Thermal = Identity
  constrained (Ambient i) = Identity (Just i)
  constrained i = Identity $ Nothing
  lconstrained i = Identity $ Nothing



isAmbient (Ambient i) = True
isAmbient i = False

-- Generic Solver | Node + Head Method
thermalPotential :: (Show a,Ord a,Floating a) => Grid Thermal a -> M.Map Int a ->M.Map Int a -> [a]
thermalPotential grid  vm nh =  term <$> l
  where
    l = links grid
    term (l,h,t,e) =   sum (thermalElement (var l vm) <$> e) - lookNode h   +   lookNode t
    lookNode h = justError "cant find node "  $ (join $ fmap (\i -> if isAmbient i then (\(Ambient i) -> Just i) $ i else Nothing) $ varM h (M.fromList $ nodesFlow grid)) <|> varM h nh
      where
        varM h = M.lookup h



thermalContinuity :: (Show a,Ord a,Floating a )=> Grid Thermal a -> M.Map Int a -> M.Map Int a -> [a]
thermalContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $ filter (not . isAmbient . snd) $ nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,h,t,_) -> h == i )
        correct i= suma . filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        genFlow _ ThermalNode  = 0
        nflow i e = genFlow (var i pm) e

thermalEq l vh = loops <> nodes
    where loops =  thermalPotential l v h
          nodes =  thermalContinuity l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh



thermalElement v (Conductor i ) = v*i
thermalElement _ (HeatFlow i) = i
