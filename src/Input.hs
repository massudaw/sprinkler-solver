{-# LANGUAGE RecursiveDo  #-}
module Input ((>~>),link,node, surface,polyhedra,runInput) where

import Sprinkler
import Hydraulic
import Domains
import Data.Functor.Identity
import Element
import Grid
import Control.Lens

import Data.Monoid
import Control.Monad.Trans.State

import Control.Monad.Fix

type ElemState = (Int,Int,Int,Int)
type InputM sys e = State (Grid sys e , ElemState)

getUniqueS,getUniqueV,getUniqueL ,getUniqueN  :: InputM sys  e Int
getUniqueN = do
  modify (over (_2._1) (+ 1))
  nd . snd <$> get

getUniqueL = do
  modify (over (_2._2) (+ 1))
  lk .snd <$> get

getUniqueS = do
  modify (over (_2._3) (+ 1))
  sf . snd <$> get

getUniqueV = do
  modify (over (_2._4) (+ 1))
  vo . snd <$> get


nd (i,_,_,_) = i
lk (_,i,_,_) = i
sf (_,_,i,_) = i
vo (_,_,_,i) = i

node  e = do
  un <-  getUniqueN
  modify (\(g,i) -> (g {nodesFlow = (un,e): nodesFlow g} ,i))
  return (un,e)

link e  (h,_) (t,_) = do
  un <-  getUniqueL
  modify (\(g,i) -> (g {links = (un,(h,t,e)): links g} ,i))
  return (un,e)

surface :: sys e -> [((Bool,Int),[sys e])] -> InputM sys e (Int,sys e)
surface e  ls  = do
  un <-  getUniqueS
  modify (\(g,i) -> (g {surfaces= (un,(fst <$> ls,e)): surfaces g } ,i))
  return (un,e)

polyhedra :: sys e -> [(Int,sys e)] -> InputM sys e (Int,sys e)
polyhedra e  ls  = do
  un <-  getUniqueS
  modify (\(g,i) -> (g {volumes = (un,(fst <$> ls,e)): volumes g } ,i))
  return (un,e)

runInput t = snd $ runState t (Grid [] [] [] [] [] [] ,(-1,0,0,0))

{-
unroll :: (Ord a,Fractional a,Show a) => [Element a] -> State (Grid Element a,(Int,Int,Int)) ()
unroll l = do
  (g,(a,b,c)) <- get
  -- runState (unrollNode (0,Open 0) l  )
  let ((_,st),((_,ai),(_,ao))) = runState (unrollNode (0,Open 0) (Origem l) ) ((Open 0,a),(Open 0 ,b))
  put (Grid [] (fmap (\(i,h,t,e) -> (i,(h,t,e))) (snd st) <> links g)[]  [] (  fst st <> nodesFlow g),(ai,ao))
-}

(>~>)
  :: MonadFix m =>
     ((t, t1) -> m (t2, t5))
     -> ((t2, t3) -> m (t4, t1)) -> (t, t3) -> m (t4, t5)
a >~> b = (\(i,l)-> mdo
          (e,f) <- a (i,n)
          (m,n) <- b (e,l)
          return (m,f) )


