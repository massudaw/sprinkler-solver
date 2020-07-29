{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Input ((>~>), link', node', link, node, surface, polyhedra, runInput, (>:>), (<|), (|>), pureL', pureL) where

import Control.Lens hiding ((<|), (|>))
import Control.Monad.Fix
import Control.Monad.Trans.State
import Domains

data ElemState = ElemState { _nodeCounter :: Int
                           , _linkCounter ::  Int
                           , _surfaceCounter :: Int
                           , _volumeCounter :: Int}
makeLenses 'ElemState

type InputM sys e = State (Grid sys e, ElemState)

getUniqueS, getUniqueV, getUniqueL, getUniqueN :: InputM sys e Int
getUniqueN = getUniqueGen nodeCounter
getUniqueL = getUniqueGen linkCounter
getUniqueS = getUniqueGen surfaceCounter
getUniqueV = getUniqueGen volumeCounter

getUniqueGen :: Lens   ElemState ElemState Int Int -> InputM sys e Int
getUniqueGen l = do
  modify (over (_2 . l ) (+ 1))
  view (_2 . l) <$> get


node' efun = do
  un <- getUniqueN
  let e = efun un
  modify (\(g, i) -> (g {nodes = (un, e) : nodes g}, i))
  return (un, e)

node :: sys e -> StateT (Grid sys e, ElemState) Identity (Int, sys e)
node e = node' (const e)

link' :: [sys e]
      -> (Int, b)
      -> (Int, b1)
      -> InputM
           sys e (Int, (Int, Int), [sys e])
link' e (h, _) (t, _) = do
  un <- getUniqueL
  modify (\(g, i) -> (g {links = (un, (h, t, e)) : links g}, i))
  return (un, (h, t), e)

link :: [sys e]
      -> (Int, b)
      -> (Int, b1)
      -> InputM
           sys e (Int, [sys e])
link e h t =  do
  (un, _ , e) <- link' e h t
  return (un ,e)

surface :: sys e -> [((Bool, Int), [sys e])] -> InputM sys e (Int, sys e)
surface e ls = do
  un <- getUniqueS
  modify (\(g, i) -> (g {surfaces = (un, (fst <$> ls, e)) : surfaces g}, i))
  return (un, e)

polyhedra :: sys e -> [((Bool, Int), sys e)] -> InputM sys e (Int, sys e)
polyhedra e ls = do
  un <- getUniqueV
  modify (\(g, i) -> (g {volumes = (un, (fst <$> ls, e)) : volumes g}, i))
  return (un, e)

runInput :: State (Grid b a, ElemState) a1
                      -> (Grid b a, ElemState)
runInput t = snd $ runState t (Grid [] [] [] [] [] [] [] [], (ElemState (-1) 0 0 0))

  
(>~>) ::
  MonadFix m =>
  ((t, t1) -> m (t2, t5)) ->
  ((t2, t3) -> m (t4, t1)) ->
  (t, t3) ->
  m (t4, t5)
a >~> b =
  ( \(i, l) -> mdo
      (e, f) <- a (i, n)
      (m, n) <- b (e, l)
      return (m, f)
  )

pureL' :: Monad m => (t -> t1 -> m t2) -> (t, t1) -> m (t2, t2)
pureL' l (i, o) = mdo
  v <- l i o
  return (v, v)

pureL :: Monad m => ((t, t1) -> m t2) -> (t, t1) -> m (t2, t2)
pureL l (i, o) = mdo
  v <- l (i, o)
  return (v, v)

(>:>) = consL

consL ::
  MonadFix m =>
  ((t, t1) -> m (t4, t2)) ->
  ((t2, t3) -> m (t1, t5)) ->
  (t, t3) ->
  m (t4, t5)
consL l m (i, o) = mdo
  (vi, vo) <- l (i, ri)
  (ri, ro) <- m (vo, o)
  return (vi, ro)

infixl 1 |>

infixr 1 <|

(|>) = leftL

(<|) = flip rightL

rightL ::
  MonadFix m =>
  (t5 -> m t3) ->
  ((t2, t3) -> m (t, t5)) ->
  t2 ->
  m t
rightL l m o = mdo
  vo <- l ri
  (ro, ri) <- m (o, vo)
  return ro

leftL ::
  MonadFix m =>
  (t -> m t2) ->
  ((t2, t3) -> m (t, t5)) ->
  t3 ->
  m t5
leftL l m o = mdo
  vo <- l ri
  (ri, ro) <- m (vo, o)
  return ro
