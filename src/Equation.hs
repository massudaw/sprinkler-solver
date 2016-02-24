module Equation where
import  Debug.SimpleReflect

displayEquation g solver =
  unlines $ fmap (\e -> show e  ++ " = 0" )$ (solver g (var . ("x" ++) . show <$> [0..] ):: [Expr] )
