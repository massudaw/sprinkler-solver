{-# LANGUAGE GADTs,FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Test where

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

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


import Language.Mecha.Export
import qualified Language.Mecha.Types as Mecha
import qualified Language.Mecha.Solid as Mecha

path i h t  l = (i,h,t,  l)

tubod l d = Tubo (Just d) l 100
jd dir d = Joelho (Just d) ("Conexao","Joelho","90")  dir 100

test3 :: (Show a ,Ord a,Floating a )=> Iteration a
test3 = Iteration ( zip (fmap (\(i,_,_,_)-> i) links) (repeat 4 )) ( zip (fmap fst nodes) (repeat 100) ) grid
  where
        grid = (Grid  links snodes nodes [] )
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
                <> patchS (57,228) (63,61)
                <> patchS  (54,226) (60,59)
                <> patchS  (51,224) (57,56)
                <> patchS  (48,222) (54,53)
                <> patchS  (45,220) (51,50)
                <> patchS  (42,218) (48,47)
                <> patchS (39,216) (45,44)
                <> patchS (36,214) (42,41)
                <> patchS  (33,212) (39,38)
                <> [ te 210 [35,14,25] 0.065 0.025, te 209 [13,14,36] 0.08 0.025
                  , te 201 [25,28,23] 0.065 0.025, sp 101 ,sp 102,te 205 [12,11,13] 0.08 0.025
                  , te 202 [23,27,21] 0.065 0.025, sp 103, sp 104, sp 105, sp 106,te 206 [16,18,12] 0.08 0.025
                  , te 203 [21,26,22] 0.065 0.025, sp 107, sp 108, sp 109, sp 110,te 207 [17,19,16] 0.08 0.025
                  , te 204 [22,24,32] 0.065 0.025, sp 111, sp 112, sp 113, sp 114,te 208 [33,20,17] 0.08 0.025
                  , (300,Open 0),(301,Open 0 ),(302,Open 0)]

        patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn + 2) (idn + 1) 0.025 (1.4 + 3*2.92) , tubo (idt +2 )  i (idn +1)   0.065 2.25, tubo   (idt +3 )  j (idn +2)        0.08 (2.25)]
        patchS  (idt,idn) (ti,tj)= [te (idn +2) [idt+3,idt+1,ti ] 0.08 0.025, te (idn +1) [tj,idt+1,idt +2] 0.065 0.025]

        links = [ (31, 212,240 , [bomba,tubod ({-20*2.89+-}2.889) 0.10 ])]
                <> [path 77 240 238 [tubod (1.0) 0.08 ,tubod 5.57 0.065 ,tubod 12.3674 0.065,jd DLeft 0.065, tubod (1.507) 0.065]]
                <> [tubo 73 237 239 0.08 1.5072,tubo 74 239 240 0.08 1.7208  ]
                <> [tubo 70 236 238 0.065 2.25 ,tubo 71 237 235 0.08 2.25 ,tubo 72 238 237 0.025 (20.6617)]
                <> [tubo 67 236 234 0.065 2.25 ,tubo 68 233 235 0.08 2.25 ,tubo 69 235 236 0.025 20.6617]
                <> [tubo 64 232 234 0.065 2.25 ,tubo 65 233 231 0.08 2.25 ,tubo 66 234 233 0.025 20.6617]
                <> [tubo 61 232 229 0.065 2.25 ,path 63 230 231  $ ($0.080) <$> [tubod 1.775 , jd DRight ,tubod 10.502 , jd DLeft ,tubod 0.475],tubo 62 231 232 0.025 20.6617]
                <> patchT (227,228) (57,228)
                <> patchT (225,226) (54,226)
                <> patchT (223,224) (51,224)
                <> patchT (221,222) (48,222)
                <> patchT (219,220) (45,220)
                <> patchT (217,218) (42,218)
                <> patchT (215,216) (39,216)
                <> patchT (213,214) (36,214)
                <> patchT (210,209) (33,212)
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
        grid = ( Grid  links snodes nodes [] )
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



testIter2 =  Iteration lflows nheads grid
  where nheads = [(1,40),(2,35),(3,30)]
        lflows =  [(1,4.5),(2,2),(3,2),(4,0.5)]
        grid  = Grid links spressure nodesFlow paths
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
testGrid = Grid links spressure nodesFlow paths
  where
    nodes = [1,2,3,4,5,6,7,8,9]
    spressure = [(8,200),(9,0)]
    nodesFlow =  [(1,Tee (TeeConfig [2,1,3] 2 (24/12) (24/12) 62.30) ),(2,Open 4 ),(3,Open 4 ),(4,Open 3),(5,Open 1),(6,Open 1),(7,Open 1)]
    links = [(1,8,1,tubo 1000 24 100),(2,1,2,tubo 2000 24 100),(3,1,6,tubo 2000 24 100),(4,2,7,tubo 1500 16 100),(5,6,5,tubo 1000 16 100),(6,7,4,tubo 1500 12 100),(7,6,7,tubo 2000 6 100),(8,5,4,tubo 2000 8 100),(9,3,2,tubo 3000 6 100),(10,3,4,tubo 3000 6 100),(11,9,3,[bomba ])]
    tubo l d c = [Tubo (Just (d/12)) (l) c]
    sp = Sprinkler (Just (13,0.2)) (Just 24) 11 1
    paths = [(1,[2,4,7,3]),(2,[7,6,8,5]),(3,[10,6,4,9]),(4,[11,9,2,1])]
    bomba = Bomba Nothing (Poly [(0,240),(2,-0.9376)]) [] []

-- main =   mainWith (fst cgrid1 ||| fst cgrid2 ||| fst cgrid3 :: Diagram B R2)
rt = do
  let iter =  solveIter testIter jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  printMatrix ( fst $ expandGrid iter)
  mainWith (assembleMap $ drawGrid  8 1 iter :: Diagram B R2)

rt1 = do
  let iter = solveIter testIter2 jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  mainWith (assembleMap $ drawGrid  212 31 iter :: Diagram B R2)

main =  do
  let  elems = elementsFHIter  iter
       iter = solveIter test3 jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  printMatrix ( fst $ expandGrid iter)
  mainWith (assembleMap $ drawGrid  212 31 iter :: Diagram B R2)
  writeFile "circle.scad" $openSCAD (assembleMap $ drawGrid  212 31 iter)


jac = (jacobian (jacobianEqNodeHeadGrid testGrid ) ) ((fmap snd $ flows testIter )++  (fmap snd $ nodeHeads testIter))

jac2 = (jacobian (jacobianEqNodeHeadGrid (grid $ testIter2) ) ) ((fmap snd $ flows testIter2 )++  (fmap snd $ nodeHeads testIter2))

jac3 = (jacobian (jacobianEqNodeHeadGrid (grid $ test3 ) ) ) (snd <$> (flows test3)++  (nodeHeads test3))
fun = (jacobianEqNodeHeadGrid testGrid ) ((fmap snd $ flows testIter )++  (fmap snd $ nodeHeads testIter))


totalHead a p va =   p/(g*rho) + v^2/(2*g)
  where g = 9.81
        rho = 1000
        v = va/a

data CyclePoint a b
  = CloseLink a
  | BranchLink b
  deriving(Eq,Show,Ord)

data DesignRegion
  = DesignRegion
  { activeNodes :: [Int]
  -- , area :: Double
  }

instance Semigroup Mecha.Solid where
  i <> j = Mecha.union i j

nodesFlowSet g = findNodesLinks g $ fmap (\n@(ni,_) -> (ni,n)) $nodesFlow $ g

enableSprinklers d (Iteration n f g)  = Iteration n f (Grid  (links g) (shead g) (fmap nodes2  (nodesFlow g)) (paths g))
  where k =S.fromList $  activeNodes d
        nodes2 (i,s@(Sprinkler _ _ _ _)) =  (i,if S.member i k then s else Open 0)
        nodes2 i = i

assembleMap (i,(_,j,l) ) = nds <> lds
  where
    nds = foldr1 (<>) $  fmap ((\(DiagramElement r a o )-> transformElement (r,a) o). snd )  $  (M.toList j)
    lds = foldr1 (<>) $  concat $ fmap (fmap (\(DiagramElement r a o )-> transformElement (r,a)  o). snd )  $  (M.toList l)


drawGrid :: Target a => Int -> Int -> Iteration Double -> ((),([Double],M.Map Int (DiagramElement a) ,M.Map Int [DiagramElement a]))
drawGrid ni li a = runState (do
                      let e = renderNode [maximum $ fmap (abs.snd) $  (flows a),0] (var ni nmap)
                      markNode ni (DiagramElement 0 (1/4) e)
                      renderGrid lmap nmap ni (0,1/4) (Left $ var li lmap ))
                        ([maximum $ fmap (abs.snd) $  (flows a),0],mempty,mempty)
  where
    lmap = M.fromList (fmap (\l@(li,_,_,_)-> (li,(var li (M.fromList (flows a)),l)) ) $ links $ grid a)
    nmap = M.fromList (findNodesLinks (grid a) $ fmap (\n@(ni,_) -> (ni,(var ni (M.fromList (nodeHeads a)),n))) $ (nodesFlow $ grid a) ++ ( fmap (\l -> Open 0) <$> shead (grid a) ))


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
