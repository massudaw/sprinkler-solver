module Utils where

infixr 5 **^

s **^ c = fmap (fmap s) c
