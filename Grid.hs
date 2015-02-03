{-# LANGUAGE DeriveFunctor,NoMonomorphismRestriction,TypeFamilies,TupleSections ,RankNTypes #-}
module Grid where

import Debug.Trace
import Data.Functor.Identity
import Control.Monad.Trans.Writer
import Control.Monad
import Data.Monoid
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


data Orientation
 = Clock
 | CounterClock
 deriving(Show)

data Grid a
  = Grid
  { links :: [(Int,Int,Int,[Element a])]
  , shead :: [(Int,a)]
  , nodesFlow :: [(Int,Element a)]
  , paths :: [(Int,[Int])]
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
{-
feet ,metric ::(Show a,Ord a,Floating a) => UnitSystem a
metric  = UnitSystem "Metric" ktubo
feet = UnitSystem "Feet" ftubo
-}

ftubo t  = perda
        where
              (Just d ) = diametroE t
              c = materialE t
              perda = 4.73*(distanciaE t)/((c**1.85)*(d**4.87))

ktubo t  = perda*10/(1000*60)**1.85
        where
              (Just d ) = diametroE t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))

-- Mass Continuity Test
continuity (Iteration q nh  (Grid  l _ f p  )) = fmap (\(i,f) -> sumn (flipped i l) + suma (correct i l)  - nflow i)  f
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        nflow i = genFlow i $ var i $ M.fromList f
        genFlow idx (Open i ) = i
        genFlow idx (Sprinkler (Just (ds,k)) _ _ _) =k -- *sqrt(var idx pm)
        suma = sum . fmap (\(li,_,_,_)-> var  li (M.fromList q) )
        sumn = sum . fmap (\(li,_,_,_)-> negate $var  li (M.fromList q) )

jacobianEqConservationGrid l vh = loops <> nodes
    where loops =  jacobianConservation  l v
          nodes =  jacobianContinuity  l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip (fmap fst $ nodesFlow l) $ drop nlinks vh

jacobianEqNodeHeadGrid l vh = loops <> nodes
    where loops =  jacobianNodeHeadEquation l v h
          nodes =  jacobianContinuity  l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh

{-
solveIterD :: (forall a . (Show a , Ord a , Floating a ) => Iteration  a ) -> (forall b. (Show b, Ord b, Floating b) => Grid b -> [b] -> [b] ) -> Iteration Double
solveIterD iter modeler =  Iteration (zip (fmap (\(i,_,_,_) -> i) $ links $ grid iter) $ take fl res) (zip (fmap fst $ nodesFlow $ grid iter)  $ drop fl res) (grid iter)
  where
    fl = length (flows iter)
    res = fst . rootJ GNewton 1e-5 100 (modeler (grid iter) ) (jacobian (modeler (grid iter)  ) )  $ (snd <$> flows iter <> nodeHeads iter )
-}

solveIter :: (forall a . (Show a , Ord a , Floating a ) => Iteration  a ) -> (forall b. (Show b, Ord b, Floating b) => Grid b -> [b] -> [b] ) -> (Iteration Double)
solveIter iter modeler =  Iteration (zip (fmap (\(i,_,_,_) -> i) $ links $ grid iter) $ take fl res) (zip (fmap fst $ nodesFlow $ grid iter)  $ drop fl res) (grid iter)
  where
    fl = length (flows iter)
    res = fst . rootJ GNewton 1e-5 100 (modeler (grid iter) ) (jacobian (modeler (grid iter)  ) )  $ (snd <$> flows iter <> nodeHeads iter )

var :: Int -> M.Map Int a -> a
var i m = case M.lookup i m of
               Just i -> i
               Nothing -> error $ "no variable " ++ show i

{-
-- Cross Method
iterGrid (Iteration l m  g) = Iteration (fmap applyCorrections l) m  g
    where flowCorrections = fmap (negate.liftA2 (/) (iterEquation l) (diffEquation l))<$>  directions
          directions = energyConservation g
          applyCorrections (l,fo) = (l, fo + sum (catMaybes $fmap (\(f,d)->  fmap (\i-> correction i $ f) $L.find ((l==). fst ) d) $zip (snd<$> flowCorrections) (snd <$> directions)))
          correction (i,(Clock,_)) = id
          correction (i,(CounterClock ,_)) = negate

diffEquation l g  = foldr1 (+) (term (M.fromList l) <$> g)
  where
    showTubo v (Bomba  _ (Poly l ) _ _ ) = negate $ foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =  0
            polyTerm (p,c) =   p*c*v**(p-1)
    showTubo v e@(Tubo _ _ _ ) = 1.85*( ftubo  e)*v**0.85
    term m (l,(Clock,(_,_,e))) =   showTubo (var l m) e
    term m (l,(CounterClock,(_,_,e))) =   showTubo (var l m) e

iterEquation l g  = foldr1 (+) (term (M.fromList l) <$> g)
  where
    showTubo v (Bomba  _ (Poly l ) _ _ ) = negate $ foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*v**p
    showTubo v e@(Tubo _ _ _) = (ftubo e)*v**1.85
    term m (l,(Clock,(_,_,e))) =   showTubo (var l m) e
    term m (l,(CounterClock,(_,_,e))) =  negate $ showTubo (var l m) e
-}

energyConservation (Grid l _ _ p  ) =  fmap (fmap pathEq) p
  where
    labelLinks l = reverse $ foldl (\ xl@(t:xs) h->  (match h t):xl) [(Clock,head l) ] (tail l)
    match (i,j,e) (d,(m,n,_))  | i == n = (Clock,(i,j,e))
    match (i,j,e) (d,(m,n,_))  | j == n = (CounterClock,(j,i,e))
    pathEq l = zip l $ labelLinks $ fmap (\li -> var  li linkMap ) l
    linkMap  = M.fromList $ fmap (\(l,h,t,e)-> (l,(h,t,e)))  l



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



-- Generic Solver | Node + Loop equation Method
jacobianConservation :: (Show a , Ord a,Floating a) => Grid a -> M.Map Int a -> [a]
jacobianConservation grid  l  = (\g -> foldr1 (+) (term l <$> g)) <$> gs
  where
    gs = snd <$> energyConservation grid
    term m (l,(Clock,(h,t,e))) = maybe id  (flip (-)) (M.lookup t  (M.fromList $ shead grid)) $maybe id  (+) (M.lookup h  (M.fromList $ shead grid)) $  sum $ pipeElement grid (var l m) <$> e
    term m (l,(CounterClock,(h,t,e))) = maybe id  (flip (-)) (M.lookup h  (M.fromList $ shead grid)) $ maybe id  (+) (M.lookup t  (M.fromList $ shead grid)) $ sum $ negate . pipeElement grid (var l m) <$> e

signedFlow :: Floating a => Grid a -> M.Map Int a ->M.Map Int (M.Map Int a)
signedFlow (Grid l _  f p ) v = M.fromList $  fmap (\(i,_) ->  (i,) $ M.fromList $ ( ( sumn $ flipped i l) ++   ((suma $ correct i l))) ) f
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> (li,var li v ))
        sumn =  fmap (\(li,_,_,_) ->  (li,negate $ var li v))


jacobianContinuity :: (Ord a,Floating a )=> Grid a -> M.Map Int a ->M.Map Int a -> [a]
jacobianContinuity (Grid l _  f p ) v pm = fmap (\(i,_) -> sum ( sumn $ flipped i l) +  (sum (suma $ correct i l))  - nflow i) f
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        nflow i = genFlow i $ var i $ M.fromList f
        genFlow idx (Open i ) = i
        genFlow idx (Tee _ ) = 0
        genFlow idx (Sprinkler (Just (ds,k)) _ _ _) = k*sqrt( abs $ var idx pm)
        suma =  fmap (\(li,_,_,_) -> var li v )
        sumn =  fmap (\(li,_,_,_) ->  negate $ var li v)


-- Generic Solver | Node + Head Method
jacobianNodeHeadEquation :: (Show a,Ord a,Floating a) => Grid a -> M.Map Int a ->M.Map Int a -> [a]
jacobianNodeHeadEquation grid  vm nh =  term <$> l
  where
    l = links grid
    sflow = signedFlow grid vm
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTee (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow grid
    addTee k = maybe 0 id (M.lookup k nodeLosses)
    term (l,h,t,e) =   sum (pipeElement grid (var l vm) <$> e) - (var h nhs )  +  addTee (h,l) + addTee (t,l)   +(var t nhs)
      where nhs = nh <> (M.fromList $shead grid)



-- Rendering System Equations
printMatrix = putStr . unlines . fmap show
{-
renderNodeHeadEquation grid l =  term l
  where
    showTubo idx (Bomba  _ (Poly l ) _ _ )  = "-(" ++ (L.intercalate " + " $ fmap  polyTerm l) ++ ")"
      where polyTerm (0,c) =  "("++ show c ++")"
            polyTerm (p,c) =  "("++ show c ++ ")*Q" ++ show idx ++ "^" ++ show p
    showTubo idx e  = "(" ++ show (tuboGrid grid e) ++ ")*Q" ++ show idx ++  "^1.85"
    term (l,h,t,e) =   showTubo l e ++ " -" ++ (maybe ("H" ++ show h ) show (M.lookup h nhs))  ++ " +" ++ (maybe ("H" ++ show t ) show (M.lookup t nhs))
      where nhs = (M.fromList $shead grid)

renderContinuity (Grid l _  f p _ ) = fmap (\(i,_) -> (i, L.intercalate  " + " (( sumn $ flipped i l)  ++ (suma $ correct i l))  ++ " = " ++ nflow i)) f
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        nflow i = genFlow i $ var i $ M.fromList f
        genFlow idx (Open i ) = show i
        genFlow idx (Sprinkler (Just (ds,k)) _ _ _) =show k ++ "*sqrt(H" ++ show idx ++ ")"
        suma =  fmap (\(li,_,_,_) -> "Q" ++ show li )
        sumn =  fmap (\(li,_,_,_) -> "(-Q" ++ show li ++ ")" )

renderEquation grid l =  L.intercalate " " $ fmap term l
  where
    showTubo idx (Bomba  _ (Poly l ) _ _ ) = "-(" ++ (L.intercalate " + " $ fmap  polyTerm l) ++ ")"
      where polyTerm (0,c) =  "("++ show c ++")"
            polyTerm (p,c) =  "("++ show c ++ ")*Q" ++ show idx ++ "^" ++ show p
    showTubo idx e = "(" ++ show (tuboGrid grid e) ++ ")*Q" ++ show idx ++  "^1.85"
    term (l,(Clock,(_,_,e))) =  "+(" ++ showTubo l e++")"
    term (l,(CounterClock,(_,_,e))) =  "-(" ++ showTubo l e ++ ")"
renderEqGrid l = unlines (loops ++ nodes ++ nhead)
    where loops = fmap (\(i,eq) -> "Loop " ++ show i ++ ": " ++ renderEquation  (grid l) eq ) ( energyConservation $ grid l )
          nhead = fmap (\eq@(i,_,_,_) -> "Pipe " ++ show i ++ ": " ++ renderNodeHeadEquation (grid l) eq ) ( links $ grid l )
          nodes = fmap (\(i,eq) -> "Node " ++ show i ++ ": " ++ eq ) (renderContinuity $ grid l)

-}


