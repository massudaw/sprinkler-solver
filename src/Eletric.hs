{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Eletric where

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.List as L
import qualified Data.Map as M
import Data.Monoid
import Domains

data Eletric a
  = Resistor
      { res :: a
      }
  | VoltageSource
      { ddp :: a
      }
  | Node
  | Ground
  deriving (Show, Eq, Ord, Functor)

instance PreSys Eletric where
  type NodeDomain Eletric = Identity
  type LinkDomain Eletric = Identity
  constrained (Ground) = Identity (Just 0)
  constrained i = Identity $ Nothing
  lconstrained i = Identity $ Nothing

-- Generic Solver | Node + Head Method
circuitPotential :: (Show a, Ord a, Floating a) => Grid Eletric a -> M.Map Int a -> M.Map Int a -> [a]
circuitPotential grid vm nh = term <$> l
  where
    l = links grid
    term (l, (h, t, e)) = sum (circuitElement (var l vm) <$> e) - lookNode h + lookNode t
    lookNode h = justError "cant find node " $ (join $ fmap (\i -> if isGround i then (\(Ground) -> Just 0) $ i else Nothing) $ varM h (M.fromList $ nodes grid)) <|> varM h nh
      where
        varM h = M.lookup h

isGround Ground = True
isGround i = False

circuitContinuity :: (Show a, Ord a, Floating a) => Grid Eletric a -> M.Map Int a -> M.Map Int a -> [a]
circuitContinuity g v pm = fmap (\(i, e) -> sum (flipped i $ links g) + (sum (correct i $ links g)) - nflow i e) $ filter (not . isGround . snd) $ nodes g
  where
    -- pipeFlow
    flipped i = sumn . filter (\(_, (h, t, _)) -> h == i)
    correct i = suma . filter (\(_, (h, t, _)) -> t == i)
    suma = fmap (\(li, (_, _, _)) -> var li v)
    sumn = fmap negate . suma
    -- nodeFlow
    genFlow _ Node = 0
    genFlow _ Ground = 0
    nflow i e = genFlow (var i pm) e

circuitEq = (\l v h -> circuitPotential l (runIdentity <$> v) (runIdentity <$> h) <> circuitContinuity l (runIdentity <$> v) (runIdentity <$> h))

circuitElement v (Resistor i) = v * i
circuitElement _ (VoltageSource i) = i
