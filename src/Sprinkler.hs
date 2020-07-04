{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Sprinkler (unrollNode, editDiametro, formatFloatN) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Tuple
import Debug.Trace
import Element
import GHC.Stack
import Hydraulic
import Numeric
import Prelude hiding (null)

unNode :: Element a -> State ((Element a, Int), (Element a, Int)) Int
unNode e = do
  modify (\((_, i), j) -> ((e, i + 1), j))
  snd . fst <$> get

unLink :: Tree a -> State ((Element a, Int), (Element a, Int)) Int
unLink (Atom e) = do
  modify (\(i, (_, j)) -> (i, (e, j + 1)))
  snd . snd <$> get

stack (Left l) (Atom (Sprinkler c _ i j) : xs) = do
  el <- fst . snd <$> get
  let e = Sprinkler c (diametroE el) i j
  un <- unNode e
  (i, r) <- stack (Right un) xs
  return $ (i, ([(un, e)], [l un []]) <> r)
stack (Right i) l@(el : xs) = do
  un <- unLink (last l)
  stack (Left (\t xs -> (un, i, t, el : xs))) xs
stack (Left f) (Atom (Joelho k (TabelaPerda jdia j l)) : xs) = do
  eo <- fst . snd <$> get
  stack (Left (\i xs -> f i (Atom (Joelho k (TabelaPerda (jdia) j l)) : xs))) xs
stack (Left f) (el : xs) = do
  stack (Left (\i xs -> f i (el : xs))) xs
stack (Left i) [] = do
  return (i, ([], []))
stack (Right i) j = error $ "no match" <> show j <> " " <> show i

diametroEJ i = diametroE i

unrollNode (ln, oe) e@(Origem i) = do
  (el, l) <- stack (Right ln) (init i)
  ul <- (snd <$> get)
  (n, r) <- unrollNode (swap ul) (last i)
  return $ (snd ul, ([], [el n []]) <> l <> r)
unrollNode (ln, oe) e@(Te _ n i j) = do
  un <- unNode (Tee undefined undefined)
  uljl <- (snd . snd <$> get)
  (elj, lj) <- stack (Right un) (init j)
  ulj <- (snd <$> get)
  (nj, rj) <- unrollNode (swap ulj) (last j)
  ulil <- (snd . snd <$> get)
  (eli, li) <- stack (Right un) (init i)
  uli <- (snd <$> get)
  (ni, ri) <- unrollNode (swap uli) (last i)

  let conf m = case n of
        TeBranch -> TeeConfig [uljl + 1, ln, ulil + 1] (fmap Circular [(diametroEJ oe), (max (diametroEJ oe) $ max (diametroEJ (headT i)) (diametroEJ (headT j))), (diametroEJ oe)]) (RoundTee 0.01 (pi / 2) m)
        TeRunL -> TeeConfig [ln, uljl + 1, ulil + 1] (fmap Circular [(diametroEJ (headT j)), (max (diametroEJ (headT j)) $max (diametroEJ (headT i)) (diametroEJ oe)), (diametroEJ (headT j))]) (RoundTee 0.01 (pi / 2) m)
        TeRunR -> TeeConfig [uljl + 1, ulil + 1, ln] (fmap Circular [(diametroEJ (headT i)), (max (diametroEJ (headT i)) $max (diametroEJ oe) (diametroEJ (headT j))), (diametroEJ (headT i))]) (RoundTee 0.01 (pi / 2) m)

  return $ (un, ([(un, Tee (traceShow (oe, (head i), (head j)) $ traceShowId $ conf 100) Table)], [elj nj [], eli ni []]) <> lj <> li <> ri <> rj)
unrollNode (ln, oe) (Atom e@(Sprinkler _ _ _ _)) = do
  un <- unNode e
  return $ (un, ([(un, e)], []))
unrollNode (ln, oe) (Atom e@(Open _)) = do
  un <- unNode e
  return $ (un, ([(un, e)], []))
unrollNode (ln, oe) (Atom e@(Reservatorio _)) = do
  un <- unNode e
  return $ (un, ([(un, e)], []))
unrollNode i j = error $ show i ++ show j

headT i = case head i of
  Atom i -> i
  j -> errorWithStackTrace "no atom"

editDiametro v (Tubo _ b c) = Tubo (Circular v) b c
editDiametro v (Joelho c (TabelaPerda _ b d)) = Joelho c (TabelaPerda (Circular v) b d)
editDiametro v i = i

formatFloatN numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""
