{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts,DeriveFunctor,TypeFamilies #-}
module Equation where
import  Debug.SimpleReflect
import Grid

displayEquation g solver =
  unlines $ fmap (\e -> show e  ++ " = 0" )$ ((prepareModel g solver)  (var . ("x" ++) . show <$> [0..] ):: [Expr] )
