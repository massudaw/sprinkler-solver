{-# LANGUAGE DeriveFoldable,TupleSections#-}
module Sprinkler (unrollNode,editDiametro ,formatFloatN )where

import Control.Monad
import Hydraulic
import Data.Tuple
import Control.Monad.Trans.State
import Numeric
import Data.Ord
import Element
import Data.Maybe
import Data.Monoid
import Prelude hiding(null)
import Debug.Trace





unNode :: Element a -> State ((Element a,Int),(Element a,Int)) Int
unNode e = do
  modify (\((_,i),j)-> ((e,i+1),j))
  snd . fst <$> get
unLink :: Element a -> State ((Element a,Int),(Element a,Int)) Int
unLink  e = do
  modify (\(i,(_,j))-> (i,(e,j+1)))
  snd . snd <$> get

stack (Left l) ((Sprinkler c _ i j):xs) = do
  el <- fst.snd <$> get
  let e = Sprinkler c (diametroE el) i j
  un <- unNode  e
  (i,r) <- stack (Right un) xs
  return $ (i,([(un,e)],[l un []]) <> r)
stack (Right i) l@(el:xs) = do
  un <- unLink (last l )
  stack (Left (\t xs -> (un,i,t,el:xs))) xs
stack (Left f ) (Joelho jdia  j k l :xs) = do
  eo <- fst.snd <$> get
  stack  (Left (\i xs -> f i (Joelho (fromMaybe (diametroE eo) (Just <$> jdia) ) j k l :xs))) xs
stack (Left f ) (el:xs) = do
  stack  (Left (\i xs -> f i (el:xs))) xs
stack (Left i) [] = do
  return (i,([],[]))
stack (Right i)  j  = error $ "no match" <>  show j <> " " <> show i


diametroEJ i = case diametroE i of
                    (Just i) -> i
                    Nothing -> error $ "sem diametro elemento " ++ show i
unrollNode (ln,oe) e@(Origem i) =  do
  (el ,l) <- stack (Right ln) (init i)
  ul <- (snd <$> get)
  (n,r) <- unrollNode   (swap ul) (last i)
  return $ (snd ul,([],[el n []]) <> l<> r)
unrollNode (ln,oe) e@(Te _ n i j ) = do
  un <- unNode e
  uljl  <-(snd .snd <$> get)
  (elj ,lj ) <- stack (Right un) (init j)
  ulj  <-(snd <$> get)
  (nj,rj) <- unrollNode  (swap ulj) (last j)
  ulil  <-(snd .snd <$> get)
  (eli ,li ) <- stack (Right un) (init i)
  uli  <-(snd <$> get)
  (ni,ri) <- unrollNode  (swap uli) (last i)

  let conf = case n of
       TeBranch -> TeeConfig [uljl +1 , ln ,ulil +1 ] 0.01 (pi/2) (diametroEJ oe) (max (diametroEJ oe ) $ max (diametroEJ (head i)) (diametroEJ (head j)))
       TeRunL -> TeeConfig [ln,uljl +1 ,ulil +1 ] 0.01 (pi/2)(diametroEJ (head j)) (max (diametroEJ (head j)) $max (diametroEJ (head i)) (diametroEJ oe))
       TeRunR -> TeeConfig [uljl +1 ,ulil +1,ln ] 0.01 (pi/2) (diametroEJ (head i)) (max (diametroEJ (head i)) $max (diametroEJ oe) (diametroEJ (head j)))

  return $ (un,([(un,Tee ( traceShow (oe ,(head i) ,(head j)) $ traceShowId $ conf 100)Table )],[elj nj [], eli ni []]) <> lj <> li  <> ri <> rj)
unrollNode (ln,oe) e@(Sprinkler _ _ _ _ ) = do
  un <- unNode e
  return $ (un,([(un,e)],[]))
unrollNode (ln,oe) e@(Open _) = do
  un <- unNode e
  return $ (un,([(un,e)],[]))
unrollNode (ln,oe) e@(Reservatorio _ ) = do
  un <- unNode e
  return $ (un,([(un,e)],[]))
unrollNode i j = error $ show i ++ show j


editDiametro v (Tubo Nothing b c )  = Tubo (Just v) b c
editDiametro v (Joelho Nothing b c d)  = Joelho (Just v) b c d
editDiametro v i = i


formatFloatN numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

