{-# LANGUAGE DeriveFunctor,NoMonomorphismRestriction,TypeFamilies,TupleSections ,RankNTypes #-}
module Grid where

import Debug.Trace
import Data.Functor.Identity
import Control.Monad.Trans.Writer
import Control.Monad
import Sprinkler
import Tee hiding (ktubo)
import Element
import Data.Reflection
import qualified Data.List  as L
import Numeric.GSL.Root
import Data.Maybe
import qualified Data.Map as M

import Control.Applicative
import Numeric.AD
import Diagrams.Prelude.ThreeD
import Control.Lens


data Orientation
 = Clock
 | CounterClock
 deriving(Show)

data Grid a
  = Grid
  { linksPosition :: [(Int,[(R3,(a,a,a))])]
  , links :: [(Int,Int,Int,[Element a])]
  , shead :: [(Int,(R3,(a,a,a)))]
  , nodesFlow :: [(Int,Element a)]
--  , origin :: (Int,R3,(a,a,a))
  }deriving(Show,Functor)


data Iteration a
  = Iteration
  { flows :: [(Int,a)]
  , nodeHeads :: [(Int,a)]
  , grid :: Grid a
  }deriving(Show,Functor)


isTee (Tee _ ) = True
isTee i = False

elementsFHIter i = (pipeFHIter i , nodeFHIter i)

nodeFHIter (Iteration vm h grid) = fmap (nodeFlowsHeads grid (M.fromList vm ) (M.fromList h) ) $ nodesFlow grid

nodeFlowsHeads grid vm h e@(ni,Tee config) = (e,fmap (\li-> (var li sflow ,  var ni h  + addTee li   ))  pipes)
  where
    tp = M.lookup  ni h
    pipes =  (teeConfig config)
    sflow =  var ni $ signedFlow grid vm
    nodeLosses = M.fromList . (\(n,Tee t) -> (\(ti,v)-> (ti,(if var ti sflow > 0 then  id else negate ) v)) <$> classifyTee ( fmap (\i -> i/1000/60) $ sflow ) t) $ e
    addTee k = maybe 0 id (M.lookup k nodeLosses)
nodeFlowsHeads grid vm h e@(ni,Sprinkler (Just (d,k)) l c _ ) = (e,[(k* sqrt (abs $ var ni h) ,var ni h )])
nodeFlowsHeads grid vm h e@(ni,Open v ) = (e,[(v,var ni h )])

pipeFHIter (Iteration vm h grid) = fmap (pipeFlowsHeads grid (M.fromList vm ) (M.fromList h) ) $ links grid

pipeFlowsHeads grid vm h e@(ni,th,tt,tubo) = (e,(var ni vm,) . pipeElement grid (var ni vm) <$>  tubo)


ktubo t  = perda*10/(1000*60)**1.85
        where
              (Just d ) = diametroE t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))

-- Mass Continuity Test
continuity (Iteration q nh g ) = fmap (\(i,f) -> sumn (flipped i (links g)) + suma (correct i (links g))  - nflow i)  (nodesFlow g)
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        nflow i = genFlow i $ var i $ M.fromList (nodesFlow g)
        genFlow idx (Open i ) = i
        genFlow idx (Reservatorio _ _ _  ) = 0
        genFlow idx (Sprinkler (Just (ds,k)) _ _ _) =k -- *sqrt(var idx pm)
        suma = sum . fmap (\(li,_,_,_)-> var  li (M.fromList q) )
        sumn = sum . fmap (\(li,_,_,_)-> negate $var  li (M.fromList q) )

jacobianEqNodeHeadGrid l vh = loops <> nodes
    where loops =  jacobianNodeHeadEquation l v h
          nodes =  jacobianContinuity  l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh


solveIter :: (forall a . (Show a , Ord a , Floating a ) => Iteration  a ) -> (forall b. (Show b, Ord b, Floating b) => Grid b -> [b] -> [b] ) -> (Iteration Double)
solveIter iter modeler =  Iteration (zip (fmap (\(i,_,_,_) -> i) $ links $ grid iter) $ take fl res) (zip (fmap fst $ nodesFlow $ grid iter)  $ drop fl res) (grid iter)
  where
    fl = length (flows iter)
    res = fst . rootJ GNewton 1e-5 100 (modeler (grid iter) ) (jacobian (modeler (grid iter)  ) )  $ (snd <$> flows iter <> nodeHeads iter )

var :: Int -> M.Map Int a -> a
var i m = case M.lookup i m of
               Just i -> i
               Nothing -> error $ "no variable " ++ show i



pipeElement grid v (Bomba  (Just (pn,vn)) (Poly l ) _ _ ) = negate $ (*pn) $ (/100)  $foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*(100*v/vn)**p
pipeElement grid v (Bomba  _ (Poly l ) _ _ ) = negate $ foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*v**p
pipeElement grid v e | v < 0 = negate $ pipeElement grid (abs v) e
pipeElement grid v e@(Resistive k p)  = k*v**p
pipeElement grid v e@(Tubo _ _ _)  = (ktubo e)*v**1.85
pipeElement grid v e@(Joelho _ _ _ _)  = (ktubo e)*v**1.85


signedFlow :: Floating a => Grid a -> M.Map Int a ->M.Map Int (M.Map Int a)
signedFlow g v = M.fromList $  fmap (\(i,_) ->  (i,) $ M.fromList $ ( ( sumn $ flipped i $ links g) ++   ((suma $ correct i $ links g))) ) (nodesFlow g)
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> (li,var li v ))
        sumn =  fmap (\(li,_,_,_) ->  (li,negate $ var li v))


jacobianContinuity :: (Ord a,Floating a )=> Grid a -> M.Map Int a ->M.Map Int a -> [a]
jacobianContinuity g v pm = fmap (\(i,_) -> sum ( sumn $ flipped i $ links g) +  (sum (suma $ correct i $ links g))  - nflow i) $ filter (not . isReservoir . snd) $ nodesFlow g
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        nflow i = genFlow i $ var i $ M.fromList $ nodesFlow g
        genFlow idx (Open i ) = i
        genFlow idx (Tee _ ) = 0
        genFlow idx (Reservatorio _ _ _ ) = 0
        genFlow idx (Sprinkler (Just (ds,k)) _ _ _) = k*sqrt( abs $ var idx pm)
        suma =  fmap (\(li,_,_,_) -> var li v )
        sumn =  fmap (\(li,_,_,_) ->  negate $ var li v)

varn h = maybe 0 id .  M.lookup h
varr3 h = maybe (r3 0)  id .  M.lookup h

-- Generic Solver | Node + Head Method
jacobianNodeHeadEquation :: (Show a,Ord a,Floating a) => Grid a -> M.Map Int a ->M.Map Int a -> [a]
jacobianNodeHeadEquation grid  vm nh =  term <$> l
  where
    l = links grid
    sflow = signedFlow grid vm
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTee (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow grid
    addTee k = maybe 0 id (M.lookup k nodeLosses)
    term (l,h,t,e) =   sum ( pipeElement grid (var l vm) <$> e) - ( varn h nh  + (realToFrac $ varr3 h nhs ^. _z) *9.81 )  +  addTee (h,l) + addTee (t,l) + ( ((realToFrac $ varr3 t nhs ^. _z) *9.81 ) + varn t nh )
      where
         nhs = fmap fst (M.fromList $shead grid)



-- Rendering System Equations
printMatrix = putStr . unlines . fmap show

