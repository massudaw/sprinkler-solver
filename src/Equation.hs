{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Equation where

import qualified Data.Foldable as F
import Data.Maybe
import Data.Functor.Compose
import Debug.SimpleReflect
import Data.Map (Map)
import Grid
import Numeric
import Domains hiding (var)


renderEquationsLast :: [Expr] -> String
renderEquationsLast  =  unlines . fmap (\(ix,e) -> last $ fmap (\(ix2,e) -> show ix <> "." <> show ix2 <> ": " <> show e <> " = 0 ") (zip [0..] (reduction e)) )  . zip [0..] 

renderEquations :: [Expr] -> String
renderEquations =  unlines . concat . fmap (\(ix,e) -> fmap (\(ix2,e) -> show ix <> "." <> show ix2 <> ": " <> show e <> " = 0 ") (zip [0..] (reduction e)) )  . zip [0..] 

displayEquation :: (PreSys sys, Traversable (LinkDomain sys), Traversable (NodeDomain sys)) => Grid sys Expr -> (Grid sys Expr -> Map Int (LinkDomain sys Expr) -> Map Int (NodeDomain sys Expr) -> [Expr]) -> [Expr] 
displayEquation  g solver =
   ((prepareModel2 g solver) (var . ("l" ++) . show <$> [0 ..]) (var . ("n" ++) . show <$> [0 ..]):: [Expr])

renderResults :: (Foldable (NodeDomain c) , Foldable (LinkDomain c), RealFloat a) => Iteration p c a  -> String
renderResults iter =  
      unlines $ ((\(ix,v) -> "n" <> show ix <> "= " <> showFFloat (Just 4)v "") <$>  zip [0..] (catMaybes (foldMap (F.toList . getCompose) $  snd <$> (pressures iter)) )) <> 
      ((\(ix,v) -> "l" <> show ix <> "= " <> showFFloat (Just 4) v "") <$>  zip [0..] (catMaybes (foldMap (F.toList . getCompose) $ snd <$> (flows iter)) ))

