{-# LANGUAGE FlexibleContexts,DeriveFunctor,NoMonomorphismRestriction,TypeFamilies,TupleSections ,RankNTypes #-}
module Grid where

import qualified Data.List as L
import Data.Functor.Compose
import Domains
import Control.Arrow
import Debug.Trace
import Data.Maybe
import qualified Data.Foldable as F
import Linear.Vector
import Control.Monad.State
import Control.Applicative
import Linear.Matrix
import Control.Monad
import Rotation.SO3
import Data.Monoid
import GHC.Stack
import Numeric.GSL.Root
import qualified Data.Map as M

import Numeric.AD
import Control.Lens
import Linear.V3






-- printResidual iter@(ForceIter f fl a) modeler = modeler a  (concat $ (\(i,j) ->  catMaybes $ F.toList i <> F.toList j). snd <$> f )
printResidual iter@(Iteration fl f a) modeler = modeler a  (inNodes <> inLinks)
  where
    inNodes = (concat $ ( catMaybes . F.toList . getCompose ). snd <$> f )
    inLinks= (concat $ (catMaybes . F.toList . getCompose ) .  snd <$> fl )


solveIter :: forall c . (Traversable (NodeDomain c) , Traversable (LinkDomain c) , PreSys c )=> (forall  a . (Show a , Ord a , Floating a ) => Iteration  c a ) -> (forall   b. (Show b, Ord b, Floating b) => Grid c b -> [b] -> [b] ) -> Iteration c Double
solveIter iter@(Iteration fl f g) modeler =  Iteration outLinks outNodes (grid iter)
  where
    (outNodes ,outLinks )= (fst $ runState  ((,) <$> nodesOutP g <*> linksOutP g) res)
    nodesOutP g = traverse (traverse (fmap Compose . uarseT  .constrained) ) (nodesFlow g)
    linksOutP g = traverse (traverse ( fmap Compose . traverse uarse .lconstrained) ) (fmap (\(i,_,_,p)-> (i,p)) $ links g)
    inNodes ,inLinks :: [Double]
    inNodes = (concat $ ( catMaybes . F.toList . getCompose ). snd <$> f )
    inLinks= (concat $ (catMaybes . F.toList . getCompose ) .  snd <$> fl )
    res = fst . rootJ HybridJ 1e-4 100 (modeler (grid iter) ) (jacobian (modeler (grid iter)  ) )  $ inNodes <> inLinks



-- Rendering System Equations
printMatrix :: Show a => [a] -> IO ()
printMatrix  = putStr . unlines . fmap show

