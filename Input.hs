{-# LANGUAGE TupleSections,RecursiveDo,DeriveFunctor,GeneralizedNewtypeDeriving #-}
module Input (link,node,runInput) where
import Control.Monad
import Control.Monad.Trans.State

import Control.Monad.Fix
import Element
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import Data.Traversable(traverse)
import Data.Maybe
import Linear.V3
import Linear.Quaternion

import qualified Data.List as L

-- getUniqueL :: Constr  Int
getUniqueL = do
  modify (\(i,(l,j)) -> (i,(l,j + 1)))
  snd.snd <$> get

-- getUniqueN :: Constr  Int
getUniqueN = do
  modify (\(i,(j,l)) -> (i,(j + 1,l)))
  fst . snd <$> get

node  e = do
  un <-  getUniqueN
  modify (\(Grid lp l np n,i) -> (Grid lp l np ((un,e): n)   ,i))
  return un

link e h t = do
  un <-  getUniqueL
  modify (\(Grid lp l np n,i) -> (Grid lp ((un,h,t,e): l) np n  ,i))
  return un

main = do
  print $ runState test1 (Grid [] [] [] [] ,(0,0))

runInput t = snd $ runState t (Grid [] [] [] [] ,(-1,0))


test1 = mdo
  let t = Open (0 ::Int)
  let tl = [Open (0 ::Int) ]
  n1 <-node t
  j <- link tl n1 n2
  n2 <- node t
  l <- link tl n2 n3
  n3 <- node t
  i <- link tl n3 n1
  return ()




