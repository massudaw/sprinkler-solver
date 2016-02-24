{-# LANGUAGE DeriveFunctor,DeriveFoldable #-}
module Eletric where

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


