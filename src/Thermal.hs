{-# LANGUAGE DeriveFunctor,DeriveFoldable #-}
module Thermal where

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


