module Utils where

import Data.Functor.Classes
infixr 5 **^

s **^ c = fmap (fmap s) c



show1 :: (Show1 f , Show a) => f a -> String
show1 i = showsPrec1 0 i ""
