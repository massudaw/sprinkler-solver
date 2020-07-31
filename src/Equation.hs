{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Equation where

import Debug.SimpleReflect
import Data.Map (Map)
import Grid
import Domains hiding (var)

displayEquation :: (PreSys sys, Traversable (LinkDomain sys), Traversable (NodeDomain sys)) => Grid sys Expr -> (Grid sys Expr -> Map Int (LinkDomain sys Expr) -> Map Int (NodeDomain sys Expr) -> [Expr]) -> String
displayEquation g solver =
  unlines $ fmap (\e -> show e ++ " = 0") $ ((prepareModel g solver) (var . ("x" ++) . show <$> [0 ..]) :: [Expr])
