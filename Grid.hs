{-# LANGUAGE NoMonomorphismRestriction,TypeFamilies,TupleSections ,RankNTypes #-}
module Grid where

import Debug.Trace
import Data.Functor.Identity
import Control.Monad.Trans.Writer
import Control.Monad
import Data.Monoid
import Sprinkler
import Tee
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
  {
   links :: [(Int,Int,Int,[Element a])]
  , shead :: [(Int,a)]
  , nodesFlow :: [(Int,Element a)]
  , paths :: [(Int,[Int])]
  , unitSystem :: UnitSystem a
  }deriving(Show)

isTee (Tee _ ) = True
isTee i = False

data Iteration a
  = Iteration
  { flows :: [(Int,a)]
  , nodeHeads :: [(Int,a)]
  , grid :: Grid a
  }deriving(Show)


data CircuitDrawer a
  = DrawerState
  { nodesState :: [(Int,Element a)]
  , linksState :: [(Int,Int,Int,Element a)]
  , nodeId :: Int
  , linkId :: Int
  }

{-
patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn +1) (idn + 2) 0.025 (1.4 + 3*2.92) , tubo (idt +2 ) i (idn +1)  0.065 4.0, tubo (idt +3 ) j (idn +2)  0.065 4.0]
patchS (i,j) (idt,idn) (ti,tj)= [te (idn +2) [idt +3,idt +1,ti] 0.065 0.025, te (idn +1) [idt +2,idt +1,tj] 0.065 0.025]
patchS' c (i,j) (ti,tj) = CircuitDrawer
      (nodeState c ++  [te (idn +2) [idt +3,idt +1,ti] 0.065 0.025, te (idn +1) [idt +2,idt +1,tj] 0.065 0.025])
      (linkState c ++ [tubo (idt + 1) (idn +1) (idn + 2) 0.025 (1.4 + 3*2.92) , tubo (idt +2 ) i (idn +1)  0.065 4.0, tubo (idt +3 ) j (idn +2)  0.065 4.0])
      (nodeId c +2)
      (linkId c +3)

path i h t  l = (i,h,t,  l)

tubod l d = Tubo (Just d) l 100
jd d = Joelho (Just d) ("Conexao","Joelho","90")  DRight 100

test3 :: (Show a ,Ord a,Floating a )=> Iteration a
test3 = Iteration ( zip (fmap (\(i,_,_,_)-> i) links) (repeat 3 )) ( zip (fmap fst nodes) (repeat 100) ) grid
  where
        grid = ( Grid  links snodes nodes [] metric)
        sp i = (i,Sprinkler (Just (0.013,8))  (Just 0.025) 12 6.1)
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
        bomba = Bomba (Just (240,800)) (bombaSF ) [] []
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (1000)))
        snodes = [(212,0)]
        nodes = [ te 211 [30,31,29] 0.08 0.08]

                <> [te 237 [71,72,30] 0.08 0.025 , te 238 [70,72,29]  0.065 0.025
                  , te 235 [68,69,71] 0.08 0.025 , te 236 [67,69,70]  0.065 0.025
                  , te 233 [65,66,68] 0.08 0.025 , te 234 [64,66,67]  0.065 0.025
                  , te 231 [63,62,65] 0.08 0.025 , te 232 [61,62,64]  0.065 0.025]
                <> patchS (227,228) (57,228) (63,61)
                <> patchS (225,226) (54,226) (60,59)
                <> patchS (223,224) (51,224) (57,56)
                <> patchS (221,222) (48,222) (54,53)
                <> patchS (219,220) (45,220) (51,50)
                <> patchS (217,218) (42,218) (48,47)
                <> patchS (215,216) (39,216) (45,44)
                <> patchS (213,214) (36,214) (42,41)
                <> patchS (210,209) (33,212) (39,38)
                <> [ te 210 [35,14,25] 0.065 0.025, te 209 [36,14,13] 0.08 0.025
                , te 201 [25,28,23] 0.065 0.025, sp 101 ,sp 102,te 205 [13,11,12] 0.08 0.025
                , te 202 [23,27,21] 0.065 0.025, sp 103, sp 104, sp 105, sp 106,te 206 [12,18,16] 0.08 0.025
                , te 203 [21,26,22] 0.065 0.025, sp 107, sp 108, sp 109, sp 110,te 207 [16,19,17] 0.08 0.025
                , te 204 [22,24,32] 0.065 0.025, sp 111, sp 112, sp 113, sp 114,te 208 [17,20,33] 0.08 0.025
                , (300,Open 0),(301,Open 0 )
                ]
        patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn +1) (idn + 2) 0.025 (1.4 + 3*2.92) , tubo (idt +2 ) i (idn +1)  0.065 4.0, tubo (idt +3 ) j (idn +2)  0.08 4.0]
        patchS (i,j) (idt,idn) (ti,tj)= [te (idn +2) [idt +3,idt +1,ti] 0.08 0.025, te (idn +1) [idt +2,idt +1,tj] 0.065 0.025]

        links = [ (31, 212 ,211, [bomba,tubod 1.0 0.08 ]),tubo 30 211 237 0.08 14.0,path 29 211 238 [ tubod 7.0 0.08 , tubod 7.0 0.065  ]]
                <> [tubo 70 236 238 0.065 2.25 ,tubo 71 235 237 0.08 2.25 ,tubo 72 237 238 0.025 20.6617]
                <> [tubo 67 234 236 0.065 2.25 ,tubo 68 233 235 0.08 2.25 ,tubo 69 235 236 0.025 20.6617]
                <> [tubo 64 232 234 0.065 2.25 ,tubo 65 231 233 0.08 2.25 ,tubo 66 233 234 0.025 20.6617]
                <> [tubo 61 229 232 0.065 2.25 ,path 63 230 231  $ ($0.080) <$> [tubod 2.12 , jd ,tubod 7.44 , jd ,tubod 0.465],tubo 62 231 232 0.025 20.6617]
                <> patchT (227,228) (57,228)
                <> patchT (225,226) (54,226)
                <> patchT (223,224) (51,224)
                <> patchT (221,222) (48,222)
                <> patchT (219,220) (45,220)
                <> patchT (217,218) (42,218)
                <> patchT (215,216) (39,216)
                <> patchT (213,214) (36,214)
                <> patchT (210,209) (33,212)
                <> [
                  tubo 14 210 209 0.025 (1.4 + 3*2.92)
                , tubo 25 210 201 0.065 4.0, tubo 13 209 205 0.08 4.0
                , tubo 28 201 101 0.025 (0.7 + 2*2.92) , tubo' 1 101 0.025 2.92, tubo 11 102 205 0.025 0.7
                , tubo 23 201 202 0.065 4.0, tubo 12 205 206 0.08 4.0
                , tubo 27 202 103 0.025 0.7, tubo' 2 103 0.025 2.92 ,tubo' 3 104 0.025 2.92 ,tubo' 4 105 0.025 2.92,tubo 18 206 106 0.025 0.7
                , tubo 21 202 203 0.065 4.0, tubo 16 206 207 0.08 4.0
                , tubo 26 203 107 0.025 0.7, tubo' 7 107 0.025 2.92 ,tubo' 6 108 0.025 2.92 ,tubo' 5 109 0.025 2.92,tubo 19 207 110 0.025 0.7
                , tubo 22 203 204 0.065 4.0 , tubo 17 207 208 0.08 4.0
                , tubo 24 204 111 0.025 0.7, tubo' 8 111 0.025 2.92 ,tubo' 9 112 0.025 2.92 ,tubo' 10 113 0.025 2.92,tubo 20 208 114 0.025 0.7
                , tubo 32 204 300 0.065 0.01,tubo 33 208 301 0.08 0.01 ]


test4 :: (Show a ,Ord a,Floating a )=> Iteration a
test4 = Iteration ( zip (fmap (\(i,_,_,_)-> i) links) (repeat 1 )) ( zip (fmap fst nodes) (repeat 200) ) grid
  where
        grid = ( Grid  links snodes nodes [] metric)
        sp i = (i,Sprinkler (Just (0.013,8))  (Just 0.025) 12 6.1)
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
        bomba = [Bomba (Just (240,150)) (Poly [(0,250),(2,-0.01376)]) [] []]
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (9.81*1000)))
        snodes = [(212,200)]
        nodes = [ te 211 [24,31,20] 0.065 0.065
                ,sp 111, sp 112, sp 113, sp 114]
        links = [ tubo 31 212 211 0.065 1.0
                , tubo 24 211 111  0.065 4.7, tubo' 8 111 0.025 2.92 ,tubo' 9 112 0.025 2.92 ,tubo 10 114 113 0.025 2.92,tubo 20 211 114 0.065 4.7]

      -}
{-
testConservation = do
    putStrLn $ renderEqGrid $  testIter
    let flws = take 20 $iterate iterGrid  testIter
    putStrLn $unlines $  fmap (show .continuity) flws
    putStrLn $unlines $ fmap (show )$    fmap  flows $ flws
-}
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

instance Show  (UnitSystem a )where
  show = unitName

data UnitSystem a
  = UnitSystem { unitName :: String , tubo :: Element a -> a }

feet ,metric ::(Show a,Ord a,Floating a) => UnitSystem a
metric  = UnitSystem "Metric" ktubo
feet = UnitSystem "Feet" ftubo

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
continuity (Iteration q nh  (Grid  l _ f p _ )) = fmap (\(i,f) -> sumn (flipped i l) + suma (correct i l)  - nflow i)  f
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

solveIter :: (forall a . (Show a ,Ord a,Floating a) => Iteration a) -> (forall b. (Show b, Ord b, Floating b) => Grid b -> [b] -> [b] ) -> (Iteration Double)
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
energyConservation (Grid l _ _ p _ ) =  fmap (fmap pathEq) p
  where
    labelLinks l = reverse $ foldl (\ xl@(t:xs) h->  (match h t):xl) [(Clock,head l) ] (tail l)
    match (i,j,e) (d,(m,n,_))  | i == n = (Clock,(i,j,e))
    match (i,j,e) (d,(m,n,_))  | j == n = (CounterClock,(j,i,e))
    pathEq l = zip l $ labelLinks $ fmap (\li -> var  li linkMap ) l
    linkMap  = M.fromList $ fmap (\(l,h,t,e)-> (l,(h,t,e)))  l

tuboGrid  = tubo . unitSystem

pipeElement grid v (Bomba  (Just (pn,vn)) (Poly l ) _ _ ) = negate $ (*pn) $ (/100)  $foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*(100*v/vn)**p
pipeElement grid v (Bomba  _ (Poly l ) _ _ ) = negate $ foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*v**p
pipeElement grid v e | v < 0 = negate $ pipeElement grid (abs v) e
pipeElement grid v e@(Resistive k p)  = k*v**p
pipeElement grid v e@(Tubo _ _ _)  = (tuboGrid grid e)*v**1.85
pipeElement grid v e@(Joelho _ _ _ _)  = (tuboGrid grid e)*v**1.85



-- Generic Solver | Node + Loop equation Method
jacobianConservation :: (Show a , Ord a,Floating a) => Grid a -> M.Map Int a -> [a]
jacobianConservation grid  l  = (\g -> foldr1 (+) (term l <$> g)) <$> gs
  where
    gs = snd <$> energyConservation grid
    term m (l,(Clock,(h,t,e))) = maybe id  (flip (-)) (M.lookup t  (M.fromList $ shead grid)) $maybe id  (+) (M.lookup h  (M.fromList $ shead grid)) $  sum $ pipeElement grid (var l m) <$> e
    term m (l,(CounterClock,(h,t,e))) = maybe id  (flip (-)) (M.lookup h  (M.fromList $ shead grid)) $ maybe id  (+) (M.lookup t  (M.fromList $ shead grid)) $ sum $ negate . pipeElement grid (var l m) <$> e

signedFlow :: Floating a => Grid a -> M.Map Int a ->M.Map Int (M.Map Int a)
signedFlow (Grid l _  f p _) v = M.fromList $  fmap (\(i,_) ->  (i,) $ M.fromList $ ( ( sumn $ flipped i l) ++   ((suma $ correct i l))) ) f
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> (li,var li v ))
        sumn =  fmap (\(li,_,_,_) ->  (li,negate $ var li v))


jacobianContinuity :: (Ord a,Floating a )=> Grid a -> M.Map Int a ->M.Map Int a -> [a]
jacobianContinuity (Grid l _  f p _) v pm = fmap (\(i,_) -> sum ( sumn $ flipped i l) +  (sum (suma $ correct i l))  - nflow i) f
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


