module Search (connected) where

import Data.Graph
import qualified Data.Set as S
import qualified Data.Array as A

-- Find if two points are connecting using the neighbors list
connected :: A.Ix a => a -> a -> A.Array a [a] -> [[(a, a)]]
connected x y g = helper x y g (S.singleton x)
  where
    helper a b g visited
      | a == b = [[]]
      | otherwise = [(a, c) : path | c <- next, path <- helper c b g nextS]
      where
        next = filter (not . (`S.member` visited)) (g A.! a)
        nextS = foldr S.insert visited next
