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
printResidual iter@(Iteration fl f a) modeler = (prepareModel a modeler )   (inNodes <> inLinks)
  where
    inNodes = (concat $ ( catMaybes . F.toList . getCompose ). snd <$> f )
    inLinks= (concat $ (catMaybes . F.toList . getCompose ) .  snd <$> fl )


solveIter :: forall c . (Traversable (NodeDomain c) , Traversable (LinkDomain c) , PreSys c )=> (forall  a . (Show a , Ord a , Floating a ) => Iteration  c a ) -> (forall   b. (Show b, Ord b, Floating b) => Grid c b -> M.Map Int (LinkDomain c b) -> M.Map Int (NodeDomain c b) -> [b] ) -> Iteration c Double
solveIter iter@(Iteration fl f g) modeler =  Iteration outLinks outNodes (grid iter)
  where
    (outNodes ,outLinks )= (fst $ runState  ((,) <$> nodesOutP g <*> linksOutP g) res)
    -- nodesOutP :: ( PreSys c ,Traversable (NodeDomain c) , Traversable (LinkDomain c)  )=> Grid c Double -> State [Double] [(Int,Compose (NodeDomain c) Maybe Double)]
    nodesOutP g = traverse (traverse (fmap Compose . uarseT  .constrained) ) (nodesFlow g)
    -- linksOutP :: ( PreSys c ,Traversable (NodeDomain c) , Traversable (LinkDomain c) , Num a )=> Grid c Double -> State [Double] [(Int,Compose (LinkDomain c) Maybe Double )]
    linksOutP g = traverse (traverse ( fmap Compose . traverse uarse .lconstrained) ) (fmap (\(i,_,_,p)-> (i,p)) $ links g)
    inNodes ,inLinks :: [Double]
    inNodes = (concat $ ( catMaybes . F.toList . getCompose ). snd <$> f )
    inLinks= (concat $ (catMaybes . F.toList . getCompose ) .  snd <$> fl )
    res = fst . rootJ HybridsJ 1e-7 1000 mod  (jacobian ( prepareModel (grid iter) modeler) )  $ inNodes <> inLinks
    mod = prepareModel (grid iter) modeler

{-solveIter iter modeler =  Iteration (zip (fmap (\(i,_,_,_) -> i) $ links $ grid iter) $ take fl res) (zip (fmap fst $ nodesFlow $ grid iter)  $ drop fl res) (grid iter)
  where
    fl = length (flows iter)
    res = fst . rootJ HybridsJ 1e-7 1000 (modeler (grid iter) ) (jacobian (modeler (grid iter)  ) )  $ (snd <$> flows iter <> pressures iter )
-}


-- Rendering System Equations
printMatrix :: Show a => [a] -> IO ()
printMatrix  = putStr . unlines . fmap show

