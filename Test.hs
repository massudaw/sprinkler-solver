{-# LANGUAGE GADTs,FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}

import Grid
import Debug.Trace
import Lint
import Data.Maybe
import Sprinkler
import Tee
import Element
import Numeric.AD
import qualified Data.Map as M
import Control.Applicative
import qualified Data.List as L
import qualified Data.Set as S
import Data.Ord
import Control.Monad.Trans.State
import Control.Monad
import Diagram

import Data.Traversable (traverse)

import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text

path i h t  l = (i,h,t,  l)

tubod l d = Tubo (Just d) l 100
jd dir d = Joelho (Just d) ("Conexao","Joelho","90")  dir 100

test3 :: (Show a ,Ord a,Floating a )=> Iteration a
test3 = Iteration ( zip (fmap (\(i,_,_,_)-> i) links) (repeat 4 )) ( zip (fmap fst nodes) (repeat 100) ) grid
  where
        grid = ( Grid  links snodes nodes [] metric)
        sp i = (i,Sprinkler (Just (0.013,8))  (Just 0.025) 12 6.1)
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
        bomba = Bomba (Just (300,1166)) (bombaSF ) [] []
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (100)))
        snodes = [(212,9.81*20*2.89)]
        nodes = [te 239 [73,74,75] 0.08 0.08 , te 240 [77,31,74] 0.08 0.08]
                <> [ te 237 [71,72,73] 0.08 0.025 , te 238 [77,72,70]  0.065 0.025
                  , te 235 [68,69,71] 0.08 0.025 , te 236 [70,69,67]  0.065 0.025
                  , te 233 [65,66,68] 0.08 0.025 , te 234 [67,66,64]  0.065 0.025
                  , te 231 [63,62,65] 0.08 0.025 , te 232 [64,62,61]  0.065 0.025]
                <> patchS (227,228) (57,228) (63,61)
                <> patchS (225,226) (54,226) (60,59)
                <> patchS (223,224) (51,224) (57,56)
                <> patchS (221,222) (48,222) (54,53)
                <> patchS (219,220) (45,220) (51,50)
                <> patchS (217,218) (42,218) (48,47)
                <> patchS (215,216) (39,216) (45,44)
                <> patchS (213,214) (36,214) (42,41)
                <> patchS (210,209) (33,212) (39,38)
                -- <>  [te (214) [39,34 ,36] 0.08 0.025, te (213) [38,34,35] 0.065 0.025]
                <> [ te 210 [35,14,25] 0.065 0.025, te 209 [13,14,36] 0.08 0.025
                  , te 201 [25,28,23] 0.065 0.025, sp 101 ,sp 102,te 205 [12,11,13] 0.08 0.025
                  , te 202 [23,27,21] 0.065 0.025, sp 103, sp 104, sp 105, sp 106,te 206 [16,18,12] 0.08 0.025
                  , te 203 [21,26,22] 0.065 0.025, sp 107, sp 108, sp 109, sp 110,te 207 [17,19,16] 0.08 0.025
                  , te 204 [22,24,32] 0.065 0.025, sp 111, sp 112, sp 113, sp 114,te 208 [33,20,17] 0.08 0.025
                  , (300,Open 0),(301,Open 0 ),(302,Open 0)]

        patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn + 2) (idn + 1) 0.025 (1.4 + 3*2.92) , tubo (idt +2 )  i (idn +1)   0.065 2.25, tubo   (idt +3 )  j (idn +2)        0.08 (2.25)]
        patchS (i,j) (idt,idn) (ti,tj)= [te (idn +2) [idt+3,idt+1,ti ] 0.08 0.025, te (idn +1) [tj,idt+1,idt +2] 0.065 0.025]

        links = [ (31, 212,240 , [bomba,tubod ({-20*2.89+-}2.889) 0.10 ])]
                <> [path 77 240 238 [tubod (1.0) 0.08 ,tubod 5.57 0.065 ,tubod 12.3674 0.065,jd DLeft 0.065, tubod (1.507) 0.065]]
                <> [tubo 73 237 239 0.08 1.5072,tubo 74 239 240 0.08 1.7208  ]
                <> [tubo 70 236 238 0.065 2.25 ,tubo 71 237 235 0.08 2.25 ,tubo 72 238 237 0.025 (20.6617)]
                <> [tubo 67 236 234 0.065 2.25 ,tubo 68 233 235 0.08 2.25 ,tubo 69 235 236 0.025 20.6617]
                <> [tubo 64 232 234 0.065 2.25 ,tubo 65 233 231 0.08 2.25 ,tubo 66 234 233 0.025 20.6617]
                <> [tubo 61 232 229 0.065 2.25 ,path 63 230 231  $ ($0.080) <$> [tubod 1.775 , jd DRight ,tubod 10.502 , jd DLeft ,tubod 0.475],tubo 62 231 232 0.025 20.6617]
                <> [tubo 58 229 230 0.025 (1.4 + 3*2.92) , tubo 59 227 229 0.065 2.25, tubo 60 230 228 0.08 2.25 ]
                <> [tubo 55 228 227 0.025 (1.4 + 3*2.92) , tubo 56 227 225 0.065 2.25, tubo 57 226 228 0.08 2.25]
                <> [tubo 52 225 226 0.025 (1.4 + 3*2.92) , tubo 53 223 225 0.065 2.25, tubo 54 226 224 0.08 2.25]
                <> [tubo 49 224 223 0.025 (1.4 + 3*2.92) , tubo 50 223 221 0.065 2.25, tubo 51 222 224 0.08 2.25]
                <> [tubo 46 221 222 0.025 (1.4 + 3*2.92) , tubo 47 219 221 0.065 2.25, tubo 48 222 220 0.08 2.25]
                <> [tubo 43 220 219 0.025 (1.4 + 3*2.92) , tubo 44 219 217 0.065 2.25, tubo 45 218 220 0.08 2.25]
                <> [tubo 40 217 218 0.025 (1.4 + 3*2.92) , tubo 41 215 217 0.065 2.25, tubo 42 218 216 0.08 2.25]
                <> [tubo 37 216 215 0.025 (1.4 + 3*2.92) , tubo 38 215 213 0.065 2.25, tubo 39 214 216 0.08 2.25]
                <> [tubo 34 213 214 0.025 (1.4 + 3*2.92) , tubo 35 210 213 0.065 2.25, tubo 36 214 209 0.08 2.25]
                -- <> patchT (213,214) (36,214)
                -- <> patchT (210,209) (33,212)
                <> [ tubo 14 209 210 0.025 (1.4 + 3*2.92)
                  , tubo 25 210 201 0.065 2.25, tubo 13 205 209 0.08 2.25
                  , tubo 28 201 101 0.025 (0.7 + 2*2.92) , tubo' 1 101 0.025 2.92, tubo 11 102 205 0.025 0.7
                  , tubo 23 202 201 0.065 2.25, tubo 12 205 206 0.08 2.25
                  , tubo 27 103 202 0.025 0.7, tubo 2 104 103 0.025 2.92 ,tubo 3 105 104 0.025 2.92 ,tubo 4 106 105 0.025 2.92,tubo 18 206 106 0.025 0.7
                  , tubo 21 202 203 0.065 2.25, tubo 16 207 206 0.08 2.25
                  , tubo 26 203 107 0.025 0.7, tubo' 7 107 0.025 2.92 ,tubo' 6 108 0.025 2.92 ,tubo' 5 109 0.025 2.92,tubo 19 110 207 0.025 0.7
                  , tubo 22 204 203 0.065 2.25  , tubo 17 207 208 0.08 2.25
                  , tubo 24 111 204 0.025 0.7, tubo 8 112 111 0.025 2.92 ,tubo 9 113 112 0.025 2.92 ,tubo 10 114 113 0.025 2.92,tubo 20 208 114 0.025 0.7
                  , tubo 32 204 300 0.065 0.4,tubo 33 208 301 0.08 0.4 ,tubo 75 239 302 0.08 0.4]


test4 :: (Show a ,Ord a,Floating a ) => Iteration a
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


{-
testConservation = do
    putStrLn $ renderEqGrid $  testIter
    let flws = take 20 $iterate iterGrid  testIter
    putStrLn $unlines $  fmap (show .continuity) flws
    putStrLn $unlines $ fmap (show )$    fmap  flows $ flws
-}

testIter2 =  Iteration lflows nheads grid
  where nheads = [(1,40),(2,35),(3,30)]
        lflows =  [(1,4.5),(2,2),(3,2),(4,0.5)]
        grid  = Grid links spressure nodesFlow paths feet
          where
            nodes = [1,2,3,4]
            spressure  = [(4,50)]
            nodesFlow = [(1,Open 0),(2,Open 2),(3,Open 2.5)]
            links = [(1,4,1,[Resistive 0.935 1.852 ]),(2,1,2,[Resistive 0.935 1.852]) ,(3,1,3,[Resistive 0.935 1.852]),(4,2,3,[Resistive 0.935 1.852])]
            paths = [(1,[1,3,2])]

testIter :: (Show a,Ord a,Floating a) => Iteration a
testIter = Iteration linkFlow nodeHeads testGrid
  where
    nodeHeads = [(1,198),(2,193),(3,195),(4,175),(5,188),(6,190),(7,184)]
    linkFlow = [(1,20),(2,9),(3,11),(4,6),(5,5.4),(6,3.5),(7,0.5),(8,0.5),(9,1),(10,1),(11,8)]

testGrid :: (Show a,Ord a,Floating a) =>  Grid a
testGrid = Grid links spressure nodesFlow paths feet
  where
    nodes = [1,2,3,4,5,6,7,8,9]
    spressure = [(8,200),(9,0)]
    nodesFlow =  [(1,Tee (TeeConfig [2,1,3] 2 (24/12) (24/12) 62.30) ),(2,Open 4 ),(3,sp),(4,sp),(5,sp),(6,sp),(7,sp)]
    links = [(1,8,1,tubo 1000 24 100),(2,1,2,tubo 2000 24 100),(3,1,6,tubo 2000 24 100),(4,2,7,tubo 1500 16 100),(5,6,5,tubo 1000 16 100),(6,7,4,tubo 1500 12 100),(7,6,7,tubo 2000 6 100),(8,5,4,tubo 2000 8 100),(9,3,2,tubo 3000 6 100),(10,3,4,tubo 3000 6 100),(11,9,3,[bomba ])]
    tubo l d c = [Tubo (Just (d/12)) (l) c]
    sp = Sprinkler (Just (13,0.2)) (Just 24) 11 1
    paths = [(1,[2,4,7,3]),(2,[7,6,8,5]),(3,[10,6,4,9]),(4,[11,9,2,1])]
    bomba = Bomba Nothing (Poly [(0,240),(2,-0.9376)]) [] []

main =   mainWith (fst cgrid1 ||| fst cgrid2 ||| fst cgrid3 :: Diagram B R2)
rt = elementsFHIter $ solveIter testIter jacobianEqNodeHeadGrid

rt1 = do
  let iter = solveIter testIter2 jacobianEqNodeHeadGrid
  mainWith (fst $ drawGrid  iter :: Diagram B R2)

rt3 =  do
  let  elems = elementsFHIter  iter
       iter = solveIter test3 jacobianEqNodeHeadGrid
  printMatrix ( fst $ expandGrid iter)
  mainWith (fst $ drawGrid  iter :: Diagram B R2)

jac = (jacobian (jacobianEqNodeHeadGrid testGrid ) ) ((fmap snd $ flows testIter )++  (fmap snd $ nodeHeads testIter))

jac2 = (jacobian (jacobianEqNodeHeadGrid (grid $ testIter2) ) ) ((fmap snd $ flows testIter2 )++  (fmap snd $ nodeHeads testIter2))

jac3 = (jacobian (jacobianEqNodeHeadGrid (grid $ test3 ) ) ) (snd <$> (flows test3)++  (nodeHeads test3))
fun = (jacobianEqNodeHeadGrid testGrid ) ((fmap snd $ flows testIter )++  (fmap snd $ nodeHeads testIter))



cgrid1,cgrid2,cgrid3 :: (Diagram B R2,(S.Set Int,S.Set Int ,M.Map Int (P2,Double)))
cgrid2 = runState (renderGrid emap (11 :: Int)  0 t1) (mempty,mempty,M.singleton 121 (p2 (0,0) , 0))
  where t1 = Right (S.empty ,(121,Tee (TeeConfig [12,10,11] 1 1 1 1 )) :: (Int,Element Double))
        emap = (fmap (fmap (S.empty,))) $ M.fromList
                [(10 , Left (10,121,122,[Tubo  Nothing 1 0 :: Element Double])),(12, Left (12,121,123,[Tubo  Nothing 2 0]))
                ,(123,Right (123,Open 1 ))
                ,(124,Right (124,Open 1 :: Element Double))
                ,(122,Right (122,Tee (TeeConfig [13,10,14] 1 1 1 1)))
                ,(124,Right (124,Tee (TeeConfig [13,15,14] 1 1 1 1)))
                ,(13 ,Left (13,124,122,[Tubo Nothing 1 0 , Joelho undefined undefined DRight undefined  ,Tubo Nothing 1 0 , Joelho undefined undefined DRight undefined  ,Tubo Nothing 1 0] ))
                ,(14 ,Left (14,124,122,[Tubo Nothing 1 0 , Joelho undefined undefined DLeft undefined  ,Tubo Nothing 1 0, Joelho undefined undefined DLeft undefined  ,Tubo Nothing 1 0 ] ))
                , (15,Left (15,124,125,[Tubo Nothing 0.5 0]))
                ,(125,Right (125,Open 1 ))]


cgrid1 = st
  where st = runState (renderGrid emap (11 :: Int)  0 t1)  (mempty,mempty,M.singleton 121 (p2 (0,0),0))
        t1 = Right (S.empty ,(121,Tee (TeeConfig [12,10,11] 1 1 1 1 )) :: (Int,Element Double))
        emap = (fmap (fmap (S.empty,))) $ M.fromList
                [(10 , Left (10,121,122,[Tubo  Nothing 1 0 :: Element Double])),(12, Left (12,121,123,[Tubo  Nothing 2 0]))
                ,(123,Right (123,Open 1 ))
                ,(124,Right (124,Open 1 :: Element Double))
                ,(122,Right (122,Tee (TeeConfig [13,10,14] 1 1 1 1)))
                ,(124,Right (124,Tee (TeeConfig [14,15,13] 1 1 1 1)))
                ,(13 ,Left (13,124,122,[Tubo Nothing 1 0 , Joelho undefined undefined DRight undefined  ,Tubo Nothing 1 0 , Joelho undefined undefined DRight undefined  ,Tubo Nothing 1 0] ))
                ,(14 ,Left (14,124,122,[Tubo Nothing 1 0 , Joelho undefined undefined DLeft undefined  ,Tubo Nothing 1 0, Joelho undefined undefined DLeft undefined  ,Tubo Nothing 1 0 ] ))
                , (15,Left (15,124,125,[Tubo Nothing 0.5 0]))
                ,(125,Right (125,Open 1 ))]

cgrid3 = runState (renderGrid emap (11 :: Int)  0 t1) (mempty,mempty,M.singleton 121 (p2 (0,0) ,0))
  where t1 = Right ((S.empty ,(121,Tee (TeeConfig [12,10,11] 1 1 1 1 )) :: (Int,Element Double)))
        emap = (fmap (fmap (S.empty,))) $M.fromList
                [(10 , Left (10,121,122,[Tubo  Nothing 1 0 :: Element Double])),(12, Left (12,121,123,[Tubo  Nothing 2 0]))
                ,(123,Right (123,Open 1 ))
                ,(124,Right (124,Open 1 :: Element Double))
                ,(122,Right (122,Tee (TeeConfig [13,10,14] 1 1 1 1)))
                ,(13 ,Left (13,122,124,[Tubo Nothing 1 0 , Joelho undefined undefined DRight undefined  ,Tubo Nothing 1 0 ] ))
                , (14,Left (14,122,125,[Tubo Nothing 1 0]))
                ,(125,Right (125,Open 1 ))]


totalHead a p va =   p/(g*rho) + v^2/(2*g)
  where g = 9.81
        rho = 1000
        v = va/a

data CyclePoint a b
  = CloseLink a
  | BranchLink b
  deriving(Eq,Show,Ord)

drawGrid :: Iteration Double -> (Diagram B R2,(S.Set Int,S.Set Int ,M.Map Int (P2,Double)))
drawGrid a = runState (renderGrid gmap 212  (1/4) (var 31 gmap ) ) (mempty,mempty,M.singleton 212 (p2 (0,0) ,1/4))
  where
    gmap = (Left <$> M.fromList (fmap (\l@(li,_,_,_)-> (li,l) ) $ links $ grid a)) <> (Right <$> M.fromList (findNodesLinks (grid a) $ fmap (\n@(ni,_) -> (ni,n)) $nodesFlow $ grid a ))


expandGrid a = runState (recurseNode  [] (lookNode (fst $ head sortedHeads))) (S.empty,S.empty)
  where
    recurseNode path t@(n,l@(s,_)) = do
       (visited,visitedNode) <- get
       let -- not visited nodes
           nextNodes = filter (not . (`S.member` visitedNode)) $ fmap (lookLinkNode n) nextLinks
           -- not visited Links
           nextLinks =  fmap (flip var linkMap) $ filter (not . (`S.member` visited )) (fmap fst $ S.toList s)
           -- return links, not visited links but visited nodes
           backLinks =  filter ((`S.member` visitedNode)  . lookLinkNode  n ) $ fmap (flip var linkMap) $ filter (not . (`S.member` visited )) (fmap fst $ S.toList s)
           fnodes = nextNodes
       modify (<> (S.map fst  s,S.fromList nextNodes))
       tnodes <- traverse (\p  -> fmap (\j -> Left (BranchLink (fromJust $ L.find (\(l,h,t,_) -> h == fst p  || t == fst p ) nextLinks )) : j) . recurseNode (n:path) $ p ) $ L.sortBy (flip (comparing (\(i,p) ->  totalHead 0.08 (var i nodePressures ) ((/2) $ sum $ fmap (abs .snd ) $ S.toList $fst p)))) $ fmap lookNode fnodes
       return  $ Right (path,totalHead 0.08 (var n nodePressures) ((/2) $ sum $ fmap (abs .snd ) $ S.toList $fst l),t) :  (fmap (Left . CloseLink ) backLinks <>  concat  tnodes)
    lookLinkNode bn (l,h,t,_) = if bn == h then t else h
    nodePressures = M.fromList $ nodeHeads a <> shead (grid a)
    lookNode i = (i,var i nodeMap)
    linkflow = M.fromList (flows a)
    sortedHeads = L.sortBy (flip (comparing (\(i,p) ->  totalHead 0.08 p ((/2) $ sum $ fmap (abs .snd ) $ S.toList $ fst $ var i nodeMap))))  (nodeHeads a)
    nodeMap =  fmap (\(s,i) -> (S.map (\si-> (si,var si linkflow)) s, i) ) $ M.fromList $findNodesLinks (grid a) $ (fmap (Left ) <$>  (shead $ grid a )) <> (fmap Right <$> (nodesFlow $ grid a))
    linkMap = M.fromList $ (\l@(i,_,_,_) -> (i,l)) <$> (links $ grid a)

findNodesLinks grid = fmap (\(i,n) -> (i,(var i nodeMapSet,n)))
    where nodeMapSet = fmap S.fromList $ M.fromListWith mappend $ concat $ (\(l,h,t,_) -> [(h,[l ]),(t,[l ])]) <$> links grid



