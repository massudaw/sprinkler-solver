{-# LANGUAGE NoMonomorphismRestriction,RecursiveDo  #-}
module Input ((>~>),link',node',link,node, surface,polyhedra,runInput,(>:>),(<|),(|>),pureL',pureL) where

import Sprinkler
import Hydraulic
import Domains
import Data.Functor.Identity
import Element
import Grid
import Control.Lens hiding ((<|),(|>))

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

node'  efun  = do
  un <-  getUniqueN
  let e = efun un
  modify (\(g,i) -> (g {nodes = (un,e): nodes g} ,i))
  return (un,e)

node  e = do
  un <-  getUniqueN
  modify (\(g,i) -> (g {nodes = (un,e): nodes g} ,i))
  return (un,e)

link' e  (h,_) (t,_) = do
  un <-  getUniqueL
  modify (\(g,i) -> (g {links = (un,(h,t,e)): links g} ,i))
  return (un,(h,t),e)

link e  (h,_) (t,_) = do
  un <-  getUniqueL
  modify (\(g,i) -> (g {links = (un,(h,t,e)): links g} ,i))
  return (un,e)

surface :: sys e -> [((Bool,Int),[sys e])] -> InputM sys e (Int,sys e)
surface e  ls  = do
  un <-  getUniqueS
  modify (\(g,i) -> (g {surfaces= (un,(fst <$> ls,e)): surfaces g } ,i))
  return (un,e)

polyhedra :: sys e -> [((Bool,Int),sys e)] -> InputM sys e (Int,sys e)
polyhedra e  ls  = do
  un <-  getUniqueS
  modify (\(g,i) -> (g {volumes = (un,(fst <$> ls,e)): volumes g } ,i))
  return (un,e)

runInput t = snd $ runState t (Grid [] [] [] [] [] [] [] [] ,(-1,0,0,0))

{-
unroll :: (Ord a,Fractional a,Show a) => [Element a] -> State (Grid Element a,(Int,Int,Int)) ()
unroll l = do
  (g,(a,b,c)) <- get
  -- runState (unrollNode (0,Open 0) l  )
  let ((_,st),((_,ai),(_,ao))) = runState (unrollNode (0,Open 0) (Origem l) ) ((Open 0,a),(Open 0 ,b))
  put (Grid [] (fmap (\(i,h,t,e) -> (i,(h,t,e))) (snd st) <> links g)[]  [] (  fst st <> nodes g),(ai,ao))
-}

(>~>)
  :: MonadFix m =>
     ((t, t1) -> m (t2, t5))
     -> ((t2, t3) -> m (t4, t1)) -> (t, t3) -> m (t4, t5)
a >~> b = (\(i,l)-> mdo
          (e,f) <- a (i,n)
          (m,n) <- b (e,l)
          return (m,f) )


pureL' :: Monad m => (t -> t1 -> m t2) -> (t, t1) -> m (t2, t2)
pureL' l (i,o) = mdo
  v <- l i o
  return (v,v)

pureL :: Monad m => ((t, t1) -> m t2) -> (t, t1) -> m (t2, t2)
pureL l (i,o) = mdo
  v <- l (i,o)
  return (v,v)

(>:>) = consL
consL
  :: MonadFix m =>
     ((t, t1) -> m (t4, t2))
     -> ((t2, t3) -> m (t1, t5)) -> (t, t3) -> m (t4, t5)
consL l m (i,o) =  mdo
  (vi,vo) <- l (i,ri)
  (ri,ro) <- m (vo,o)
  return (vi,ro)

infixl 1 |>
infixr 1 <|

(|>) = leftL
(<|) = flip rightL

rightL
  :: MonadFix m =>
     (t5 -> m t3)
     -> ((t2, t3) -> m (t, t5)) -> t2 -> m t
rightL l m o =  mdo
  vo <- l ri
  (ro,ri) <- m (o,vo)
  return ro


leftL
  :: MonadFix m =>
     (t -> m t2)
     -> ((t2, t3) -> m (t, t5)) -> t3 -> m t5
leftL l m o =  mdo
  vo <- l ri
  (ri,ro) <- m (vo,o)
  return ro


