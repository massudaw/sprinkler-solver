{-# LANGUAGE RecursiveDo  #-}
module Input ((>~>),link,node,unroll, runInput) where

import Sprinkler
import Hydraulic
import Domains
import Data.Functor.Identity
import Element
import Grid

import Data.Monoid
import Control.Monad.Trans.State

import Control.Monad.Fix


-- getUniqueL :: Constr  Int

getUniqueL
  :: State (a1, (a, Int))  Int
getUniqueL = do
  modify (\(i,(l,j)) -> (i,(l,j + 1)))
  snd.snd <$> get

-- getUniqueN :: Constr  Int
getUniqueN
  :: State (a1,(Int, a))  Int
getUniqueN = do
  modify (\(i,(j,l)) -> (i,(j + 1,l)))
  fst . snd <$> get

node  e = do
  un <-  getUniqueN
  modify (\(Grid lp l np n,i) -> (Grid lp l np ((un,e): n) ,i))
  return (un,e)

link e  (h,_) (t,_) = do
  un <-  getUniqueL
  modify (\(Grid lp l np n,i) -> (Grid lp ((un,h,t,e): l) np n ,i))
  return (un,e)


runInput t = snd $ runState t (Grid [] [] [] [] ,(-1,0))

unroll :: (Ord a,Fractional a,Show a) => [Element a] -> State (Grid Element a,(Int,Int)) ()
unroll l = do
  (g,(a,b)) <- get
  -- runState (unrollNode (0,Open 0) l  )
  let ((_,st),((_,ai),(_,ao))) = runState (unrollNode (0,Open 0) (Origem l) ) ((Open 0,a),(Open 0 ,b))
  put (Grid [] ( (fmap (\(l,h,t,e)-> (l,h,t,e)) $ snd st) <> links g) [] (  fst st <> nodesFlow g),(ai,ao))


(>~>)
  :: MonadFix m =>
     ((t, t1) -> m (t2, t5))
     -> ((t2, t3) -> m (t4, t1)) -> (t, t3) -> m (t4, t5)
a >~> b = (\(i,l)-> mdo
          (e,f) <- a (i,n)
          (m,n) <- b (e,l)
          return (m,f) )


