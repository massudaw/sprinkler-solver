{-# LANGUAGE NoMonomorphismRestriction #-}

import Grid
import Lint
import Data.Monoid
import Data.Maybe
import Sprinkler
import Tee
import Numeric.AD
import qualified Data.Map as M
import Control.Applicative


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

rt = elementsFHIter $ solveIter testIter jacobianEqNodeHeadGrid
rt1 = elementsFHIter $ solveIter testIter2 jacobianEqNodeHeadGrid
rt3 =  do
  let  elems = elementsFHIter  iter
       iter = solveIter test3 jacobianEqNodeHeadGrid
  printMatrix (flows iter)
  printMatrix (nodeHeads iter)
  printMatrix (fst elems)
  printMatrix (snd elems)

jac = (jacobian (jacobianEqNodeHeadGrid testGrid ) ) ((fmap snd $ flows testIter )++  (fmap snd $ nodeHeads testIter))

jac2 = (jacobian (jacobianEqNodeHeadGrid (grid $ testIter2) ) ) ((fmap snd $ flows testIter2 )++  (fmap snd $ nodeHeads testIter2))

jac3 = (jacobian (jacobianEqNodeHeadGrid (grid $ test3 ) ) ) (snd <$> (flows test3)++  (nodeHeads test3))
fun = (jacobianEqNodeHeadGrid testGrid ) ((fmap snd $ flows testIter )++  (fmap snd $ nodeHeads testIter))

main = print jac
