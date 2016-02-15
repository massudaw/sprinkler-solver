{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Grid
import Debug.Trace
import System.Process
import Control.Monad.Fix
import Control.Monad
import Lint
import Position
import Exponential.Class
import Data.Distributive
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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Mecha
import Diagram

import Control.Lens
import Data.Traversable (traverse)

import Input
import Diagrams.Prelude hiding (end)


path i h t  l = (i,h,t,  l)

jd dir d = Joelho (Just d) ("Conexao","Joelho","90")  dir 100

makeIter i j g = Iteration ( zip (fmap (\(i,_,_,_)-> i) (links g)) (replicate 10 20 <>  repeat 4 )) ( zip ( fmap fst $ filter (not .isReservoir.snd)  (nodesFlow g))(repeat 100)) (realToFrac  <$>  upgradeGrid i j g)


grid4 :: RealFloat a => Grid a
grid4 =  Grid [] links [] nodes
  where nodes = [te 3 [1,3,2] 0.065 0.065  ,(4,Open 0 ),(1,Reservatorio 0 Nothing (Open 0)),(2,Reservatorio 0 Nothing (Open 0))]
        links = [(1,3,2,($0.065) <$> [tubod 0.2,jd $ upC 0 ,tubod 0.4])
                ,(2,3,1,($0.065) <$> [tubod 0.2,jd $ dowC 0,tubod 0.4])
                ,(3,4,3,($0.065) <$> [tubod 2,jd $ right90 ,tubod 2])]
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (100)) Table )
        tubod l d = Tubo (Just d) l 100

fokus :: RealFloat a => Grid a
fokus = (Grid [] links [] nodes)
  where
    te i c dri dbi =  (i,Tee (TeeConfig c (0.1*dbi) dbi dri (100))Table )
    tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
    dm = 0.25
    dr= 0.10
    db = 0.065
    sp i = (i,Sprinkler (Just (16,15.1))  (Just db) 14 (0.16*60*1.7) )
    bomba = Bomba (Just (750,4000)) bombaSF [] []
    seqT (ti,tj) (idt,idn) n = concat $ fmap (seq (ti,tj) (idt,idn)) [0..n]
      where
        seq (ti,tj) (idt,idn) i = patchT (ti+ i*2 ,tj +i*2) (idt + i*3,idn + i*2)
    seqS (ti,tj) (idt,idn) n = concat $ fmap (seq (ti,tj) (idt,idn)) [0..n]
      where
        seq (ti,tj) (idt,idn) i = patchS (ti+ i*3 ,tj +i*3) (idt + i*3,idn + i*2)
    patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn + 2) (idn + 1) db 49.26 , tubo (idt +2 )  i (idn +1)   dr 3.7, tubo   (idt +3 )  j (idn +2)        dr 3.7]
    patchS (ti,tj)(idt,idn) = [te (idn +2) [ti,idt+1, idt+3 ] dr db, te (idn +1) [idt+2,idt+1,tj] dr db]
    nodes = [
            te 1 [1,2,3] dr dr
            ,te 2 [5,4,3] dr db
            ,te 3 [6,4,5] dr db
            ,te 4 [8,7,6] dr db
            ,te 5 [1,7,9] dr db]
            <> seqS (9,8) (9,5) 11
            <> [ te 30 [49,51,44] dr db
            , sp 101, sp 102, sp 103
            , te 31 [45,48,50] dr db]
            <> [ te 32 [56,52,49] dr db
            , sp 104, sp 105, sp 106
            , te 33 [50,55,57] dr db]
            <> [ te 34 [63,58,56] dr db
            , sp 107, sp 108, sp 109, sp 110
            , te 35 [57,62,64] dr db]
            <> [ te 36 [70,65,63] dr db
            , sp 111, sp 112, sp 113, sp 114
            , te 37 [64,69,70] dr db,(0,Reservatorio 2 Nothing (Open 0))]

    tubod l d = Tubo (Just d) l 100
    links = [(2,0,1,editDiametro dm <$> [tubod 0.1 dm ,joelhoD,tubod 0.5 dm,tubod 1 dm,joelhoU0,tubod 2.726 dm ,joelhoL,tubod  1.0 dm,bomba,tubod  1 dm,joelhoUV (-1/4),tubod  1 dm,joelhoD ,tubod  0.76 dm,joelhoL,tubod  3.72 dm,joelhoD,tubod 1 dm, joelhoU0,tubod  0.2 dm,joelhoU0,joelhoL , tubod  1  dm,joelhoD ,tubod 3.6 dm ,joelhoL , tubod  22.3 dm,joelho,tubod  21.66  dm,joelhoL ,tubod 12.75 dm , tubod  62.46   dm,joelho,tubod  0.768 dm,jd (upC 0) dm , tubod 11 dm ,jd (dowC 0 ) dm ,tubod 2.889 dm,tubod 0.1 dr])
            ,tubo 3 1 2 dr 1.8
            ,tubo 4 2 3 db 49.26
            ,(5,2,3,editDiametro dr <$> [tubod 1.54 dr,joelhoR,tubod 49.26 dr ,joelhoR,tubod 1.54 dr])
            ,tubo 6 3 4 dr 3.59
            ,tubo 1 5 1 dr 1.79
            ,tubo 7 4 5 db 49.26
            ,tubo 8 4 6 dr 3.7
            ,tubo 9 5 7 dr 3.7]
            <> seqT (8,9) (9,5) 11
            <> [tubo 51 30 101 db  1.8
              ,tubo 46 101 102 db  3.7
              ,tubo 47 102 103 db  3.75
              ,tubo 48 103 31 db  (49.26 - 3.7 - 1.8 - 3.75)
              ,tubo 49 30 32 dr 3.7
              ,tubo 50  31 33 dr 3.7]
            <> [tubo 52 32 104 db  1.8
            ,tubo 53 104 105 db  3.7
            ,tubo 54 105 106 db  3.75
            ,tubo 55 106 33 db  (49.26 - 3.7 - 1.8 - 3.75)
            ,tubo 56 32 34 dr 3.7
            ,tubo 57 33 35 dr 3.7]
            <> [tubo 58 34 107 db  1.8
            ,tubo 59 107 108 db  3.7
            ,tubo 60 108 109 db  3.75
            ,tubo 61 109 110 db  3.75
            ,tubo 62 110 35 db  (49.26 - 3.7 - 1.8 - 3.75 - 3.75)
            ,tubo 63 34 36 dr 3.7
            ,tubo 64 35 37 dr 3.7]
            <> [tubo 65 36 111 db  1.8
            ,tubo 66 111 112 db  3.7
            ,tubo 67 112 113 db  3.75
            ,tubo 68 113 114 db  3.75
            ,tubo 69 114 37 db  (49.26 - 3.7 - 1.8 - 3.75 - 3.75)
            ,(70,37,36, editDiametro dr <$>  [tubod 1.42 dr,joelhoL ,tubod 49.26 dr,joelhoL,tubod 1.42 dr])]


test3 :: (Show a ,Ord a,Fractional a )=> Iteration a
test3 = Iteration ( zip (fmap (\(i,_,_,_)-> i) links) (repeat 4 )) ( zip ( fmap fst $ filter (not .isReservoir.snd)  nodes )   (repeat 100) ) (realToFrac  <$>  upgradeGrid 212 31 grid)
  where
        grid = (Grid  [] links [] nodes )
        sp i = (i,Sprinkler (Just (0.013,8))  (Just 0.025) 12 6.1)
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (100))Table)
        tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
        bomba = Bomba (Just (300,1166)) bombaSF [] []
        nodes = [te 239 [73,74,75] 0.08 0.08 , te 240 [77,31,74] 0.08 0.08]
                <> [ te 237 [71,72,73] 0.08 0.025 , te 238 [77,72,70]  0.065 0.025
                  , te 235 [68,69,71] 0.08 0.025 , te 236 [70,69,67]  0.065 0.025
                  , te 233 [65,66,68] 0.08 0.025 , te 234 [67,66,64]  0.065 0.025
                  , te 231 [63,62,65] 0.08 0.025 , te 232 [64,62,61]  0.065 0.025]
                <> patchS  (51,224) (63,61)
                <> patchS  (48,222) (54,53)
                <> patchS  (45,220) (51,50)
                <> patchS  (42,218) (48,47)
                <> patchS (39,216) (45,44)
                <> patchS (36,214) (42,41)
                <> patchS  (33,212) (39,38)
                <> [ te 210 [35,14,25] 0.065 0.025, sp 121 , te 209 [13,96,36] 0.08 0.025
                  , te 201 [25,28,23] 0.065 0.025,sp 120, sp 119, sp 118 , sp 101 ,sp 102,te 205 [12,11,13] 0.08 0.025
                  , te 202 [23,27,21] 0.065 0.025,sp 117, sp 103, sp 104, sp 105, sp 106,te 206 [16,18,12] 0.08 0.025
                  , te 203 [21,26,22] 0.065 0.025,sp 116, sp 107, sp 108, sp 109, sp 110,te 207 [17,19,16] 0.08 0.025
                  , te 204 [22,24,32] 0.065 0.025,sp 115, sp 111, sp 112, sp 113, sp 114,te 208 [33,20,17] 0.08 0.025
                  , (300,Open 0),(301,Open 0 ),(302,Open 0),(212,Reservatorio 1 Nothing (Open 0))]

        patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn + 2) (idn + 1) 0.025 (1.4 + 4*2.92) , tubo (idt +2 )  i (idn +1)   0.065 2.25, tubo   (idt +3 )  j (idn +2)        0.08 (2.25)]
        patchS (idt,idn) (ti,tj)= [te (idn +2) [idt+3,idt+1,ti ] 0.08 0.025, te (idn +1) [tj,idt+1,idt +2] 0.065 0.025]
        tubod l d = Tubo (Just d) l 100

        links = [(31,212,240 , [tubod 0.1 0.1 ,joelhoD,tubod 0.5 0.1,tubod 1 0.1,joelhoU0,tubod 2.726 0.1 ,joelhoL,tubod  1.0 0.1,Bomba (Just (300,1066)) bombaSF [] [],tubod  1 0.1,joelhoUV (-1/4),tubod  1 0.1,joelhoD ,tubod  0.76 0.1,joelhoL,tubod  3.72 0.1,joelhoD,tubod 1 0.1, joelhoU0,tubod  0.2 0.1,joelhoU0,tubod  1 0.1,joelhoD ,tubod  0.2 0.1,joelhoL,tubod  1.56 0.1,joelhoL ,tubod  4.2 0.1,joelho,tubod  0.768 0.1,jd (dowC 0 ) 0.1, tubod (4*3.24) 0.1,tubod (11*3.06) 0.1,tubod 3.06 0.1, tubod 3.06 0.1 ,tubod 2.88 0.1 ,jd (upC  $ 1/2) 0.1 ,tubod ({-20*2.89+-}2.889) 0.10 ])]
                <> [path 77 240 238 [tubod (1.0) 0.08 ,tubod 5.57 0.065 ,tubod 12.3674 0.065,jd left90 0.065, tubod (1.507) 0.065]]
                <> [tubo 73 237 239 0.08 1.5072,tubo 74 239 240 0.08 1.7208  ]
                <> [tubo 70 236 238 0.065 2.25 ,tubo 71 237 235 0.08 2.25 ,tubo 72 238 237 0.025 (20.6617)]
                <> [tubo 67 236 234 0.065 2.25 ,tubo 68 233 235 0.08 2.25 ,tubo 69 235 236 0.025 20.6617]
                <> [tubo 64 232 234 0.065 2.25 ,tubo 65 233 231 0.08 2.25 ,tubo 66 234 233 0.025 20.6617]
                <> [tubo 61 232 225 0.065 2.25 ,path 63 226 231  $ ($0.080) <$> [tubod 1.775 , jd right90  ,tubod (10.502-2.92) , jd left90 ,tubod 0.475],tubo 62 231 232 0.025 20.6617]
                <> patchT (223,224) (51,224)
                <> patchT (221,222) (48,222)
                <> patchT (219,220) (45,220)
                <> patchT (217,218) (42,218)
                <> patchT (215,216) (39,216)
                <> patchT (213,214) (36,214)
                <> patchT (210,209) (33,212)
                <> [ tubo 14 210 121 0.025 0.7, tubo 96 121 209 0.025  (0.7+ 4*2.92)
                  , tubo 25 210 201 0.065 2.25, tubo 13 205 209 0.08 2.25
                  , tubo 28 201 120 0.025 0.7 , tubo 93 120 119 0.025 2.92, tubo 94 119 118 0.025 2.92, tubo 95 118 101 0.025 2.92, tubo 1 101 102 0.025 2.92, tubo 11 102 205 0.025 0.7
                  , tubo 23 202 201 0.065 2.25, tubo 12 205 206 0.08 2.25
                  , tubo 27 103 202 0.025 0.7 , tubo 92 103 117 0.025 2.92, tubo 2 104 117 0.025 2.92 ,tubo 3 105 104 0.025 2.92 ,tubo 4 106 105 0.025 2.92,tubo 18 206 106 0.025 0.7
                  , tubo 21 202 203 0.065 2.25, tubo 16 207 206 0.08 2.25
                  , tubo 26 203 107 0.025 0.7 , tubo 91 107 116 0.025 2.92, tubo 7 116 108  0.025 2.92 ,tubo' 6 108 0.025 2.92 ,tubo' 5 109 0.025 2.92,tubo 19 110 207 0.025 0.7
                  , tubo 22 204 203 0.065 2.25, tubo 17 207 208 0.08 2.25
                  , tubo 24 111 204 0.025 0.7 , tubo 90 111 115 0.025 2.92, tubo 8 112 115 0.025 2.92 ,tubo 9 113 112 0.025 2.92 ,tubo 10 114 113 0.025 2.92,tubo 20 208 114 0.025 0.7
                  , tubo 32 204 300 0.065 0.4,tubo 33 208 301 0.08 0.4 ,tubo 75 239 302 0.08 0.4]



westpoint =
  let r1 = Origem [tubod 0.1 0.1 ,joelhoD,tubod 0.1 0.5,tubod 0.1 1,joelhoU0,tubod 0.1 2.726 ,joelhoL,tubod 0.1 1.0,Bomba (Just (300,1066)) bombaSF [] [],tubod 0.1 1,joelhoUV (-1/4),tubod 0.1 1,joelhoD ,tubod 0.1 0.76,joelhoL,tubod 0.1 3.72,joelhoD,tubod 0.1 1,joelhoU0,tubod 0.1 0.2,joelhoU0,tubod 0.1 1,joelhoD ,tubod 0.1 0.2,joelhoL,tubod 0.1 1.56,joelhoL ,tubod 0.1 4.2 ,joelho,tubod 0.1 0.768 ,joelhoD0,tubod 0.1  3.24 ,joelhoU, tubod 0.08 1.31,joelho,tubod 0.08 3.22,tee TeRunR  [tubod 0.08 0.1 ,Open 0][tubod 0.08 1.22,joelho,tubod 0.08 2.8,tee TeRunL [tubod 0.08 2.23,tee TeRunL  [ tubod 0.08 1.67 , tee TeRunR   [ tubod 0.025 0.73,sp,tubod 0.025 0.1, Open 0] r5  ] [tubod 0.08 0.1 ,Open 0] ] [tubod 0.08 0.1 ,Open 0]] ]
      r5 =[tubod 0.065 0.54,tee TeRunL  [tubod 0.05 2.96 , cruz r3 r4 [tubod 0.025 1.77,sp,tubod 0.025 0.1, Open 0]   ][tubod 0.025 0.1 , Open 0]]
      r4 = [tubod 0.05 4.0 , tee TeBranch r2 [tubod 0.025 1.77 ,sp,tubod 0.025 0.1, Open 0] ]
      r2 = [tubod 0.04 2.18 ,sp,tubod 0.04 4.0,sp,tubod 0.032  4.0 ,sp,tubod 0.025 4.0 ,sp , tubod 0.025 1.50, joelho45, tubod 0.025 1.41,sp , tubod 0.025 0.1 ,Open 0]
      r3 = [tubod 0.04 2.18 ,sp,tubod 0.04 4.0,sp,tubod 0.032 4.0 ,sp,tubod 0.025 4.0 ,sp ,tubod 0.025 2.50 ,sp,tubod 0.025 0.1,Open 0]
      sp = Sprinkler (Just (11,5.8)) Nothing 11 4.1
      st = snd $ fst $ runState (unrollNode (0,Open 0) r1 ) ((Open 0,0),(Open 0 ,0))
  in Grid [] (fmap (\(l,h,t,e)-> (l,h,t, (editDiametro 0.08) <$> e)) $ snd st) [] (  fst st <> [(0,Reservatorio 0 Nothing (Open 0))])
    where
      tubod l d = Tubo (Just d) l 100



sanmarinoSubsolo =
  let r1 = Origem $  [tubod 0.1 0.1 ,joelhoD,tubod 0.1 0.5,tubod 0.1 1,joelhoU0,tubod 0.1 2.726 ,joelhoL,tubod 0.1 1.0,Bomba (Just (300,1566)) bombaSF [] [],tubod 0.1 1,joelhoUV (-1/4),tubod 0.1 1,joelhoD ,tubod 0.1 0.76,joelhoL,tubod 0.1 3.72,joelhoD,tubod 0.1 1,joelhoU0,tubod 0.1 0.2,joelhoU0,tubod 0.1 1,joelhoD ,tubod 0.1 0.2,joelhoL,tubod 0.1 1.56,joelhoL ,tubod 0.1 4.2 ,joelho,tubod 0.1 0.768 ,joelhoD0,tubod 0.1  (20*3.24 + 3.04) ,joelhoU] <> r45
      r45 = [tubod 0.065 0.1 , hid , tubod 0.065 0.1 , hid,tubod 0.065 6.15 , tee TeRunR r3 r23]
      r23 = [tubod 0.065 4.00 , tee TeRunR r3 r2]
      r2 = [tubod 0.065 4,joelho, tubod 0.04 8.9413 , tee TeRunL [  tubod 0.025 0.4597  ,sp ,tubod 0.025 3.54, tee TeRunL  [tubod 0.025 0.4587 ,sp , tubod 0.025 0.01 ,Open 0 ][tubod 0.025 2.36  ,joelho45 , tubod 0.025 0.5402 ,sp,tubod 0.025 0.1,Open 0]] [tubod 0.025 1.96 ,joelho45 , tubod 0.025 0.5402 ,sp,tubod 0.025 0.1,Open 0] ]
      r3 = [tubod 0.04 9.40,sp,tubod 0.032 4.00 ,sp,tubod 0.025 4.00 ,sp ,tubod 0.025 3.00 ,sp,tubod 0.025 0.1,Open 0]
      sp = Sprinkler (Just (11,5.8)) Nothing 12.4 4.1
      hid = Sprinkler (Just (16,16.3)) Nothing 10 20
      tubo d = Tubo Nothing d 100
      tubod di d = Tubo (Just di) d 100
      tee b i j = Te  Nothing b i j
      cruz i j k = tee TeRunL [tubod 0.05 0.01,tee TeRunR  k j ] i
      west = ["22:11"]
      st = snd $ fst $ runState (unrollNode (0,Open 0) r1) ((Open 0,0),(Open 0 ,0))
  in Grid [] (fmap (\(l,h,t,e)-> (l,h,t, (editDiametro 0.08) <$> e)) $ snd st) [] (  fst st <> [(0,Reservatorio 0 Nothing (Open 0))])



sanmarinoTerraco =
  let r1 = Origem $  [tubod 0.1 0.1 ,joelhoD,tubod 0.1 0.5,tubod 0.1 1,joelhoU0,tubod 0.1 2.726 ,joelhoL,tubod 0.1 1.0,Bomba (Just (300,1566)) bombaSF [] [],tubod 0.1 1,joelhoUV (-1/4),tubod 0.1 1,joelhoD ,tubod 0.1 0.76,joelhoL,tubod 0.1 3.72,joelhoD,tubod 0.1 1,joelhoU0,tubod 0.1 0.2,joelhoU0,tubod 0.1 1,joelhoD ,tubod 0.1 0.2,joelhoL,tubod 0.1 1.56,joelhoL ,tubod 0.1 4.2 ,joelho,tubod 0.1 0.768 ,joelhoD0,tubod 0.1  3.24 ,joelhoU] <> r4523
      r5 = [tubod 0.065 2.69,joelho,tubod 0.065 (4.118 ) ,sp,tubod 0.04 2.35,sp,tubod 0.05 2.35 ,sp, tubod 0.025 2.35 ,sp,tubod 0.025 0.1, Open 0]
      r4 = [tubod 0.065 (3.19 + 0.92) ,sp,tubod 0.04 2.35,sp,tubod 0.05 2.35 ,sp, tubod 0.025 2.35 ,sp,tubod 0.025 0.1, Open 0]
      r4523 = [tubod 0.08 0.5 ,joelho, tubod 0.08 0.1, hid, tubod 0.08 0.1 , hid,tubod 0.08 0.479 , tee TeBranch r45 r23]
      r45 = [tubod 0.065 0.479 , tee TeRunR r4 r5]
      r23 = [tubod 0.065 5.04 , tee TeRunL r3 r2]
      r2 = [tubod 0.065 4.31,sp,tubod 0.065 3.30 ,sp,tubod 0.04 2.79 ,sp ,tubod 0.032 0.75, tee TeRunR [tubod 0.025 2.25 ,sp,tubod 0.025 0.1,Open 0] [tubod 0.025 3.16,sp , tubod 0.025 0.01 ,Open 0 ]]
      r3 = [tubod 0.065 2.32 , joelhoL ,tubod 0.065 (1.93+1.30),joelho,tubod 0.04 1.5296, joelhoL ,tubod 0.04 1.08,sp,tubod 0.032 3.30 ,sp,tubod 0.025 2.79 ,sp ,tubod 0.025 3.00 ,sp,tubod 0.025 0.1,Open 0]
      sp = Sprinkler (Just (13,5.8)) Nothing 12.4 4.1
      hid = Sprinkler (Just (16,16.3)) Nothing 10 20
      tubo d = Tubo Nothing d 100
      tubod di d = Tubo (Just di) d 100
      te i j = Te  Nothing TeBranch i j
      tee b i j = Te  Nothing b i j
      opte i j = OptTe  TeBranch i j
      cruz i j k = tee TeRunL [tubod 0.05 0.01,tee TeRunR  k j ] i
      west = ["22:11"]
      st = snd $ fst $ runState (unrollNode (0,Open 0) r1) ((Open 0,0),(Open 0 ,0))
  in Grid [] (fmap (\(l,h,t,e)-> (l,h,t, (editDiametro 0.08) <$> e)) $ snd st) [] (  fst st <> [(0,Reservatorio 0 Nothing (Open 0))])


casaMaquina pru bomba  = [tubod pru 0.1 ,joelhoD,tubod pru 0.5,tubod pru 1,joelhoU0,tubod pru 2.726 ,joelhoL,tubod pru 1.0,bomba ,tubod pru 1,joelhoUV (-1/4),tubod pru 1,joelhoD ,tubod pru 0.76,joelhoL,tubod pru 3.72,joelhoD,tubod pru 1,joelhoU0,tubod pru 0.1,joelhoU0,tubod pru 1,joelhoD ,tubod pru pru]
  where
      tubod di d = Tubo (Just di) d 100

replcomp x m = foldl1 (.)   (replicate x m)



data ProjectHeader
  = ProjectHeader { projectName :: String
    , endereco :: String
    , projectOwner :: String
    , authorInfo :: Author
    , regionInfo :: Region
    }

data Region
  = Region  { regionName :: String
    , regionFile :: String
    , regionView :: String
    }
data Author
  = Author { authorName :: String
    , creaNumber :: String
    , formation :: String
    , empresa :: String
    }

gridInput  soff =  (ph rteto , pregrid  {links = (links pregrid  ), nodesFlow = nodesFlow pregrid })
      where
        ph = ProjectHeader  "Depósito Armazém Johnson & Johnson - Galpão 01"  "\"RODOVIA BR-153, QUADRA CH, JARDIM GUANABARA, GALPÃO 01, GOIÂNIA, GOIÁS\"" "ATLAS LOGÍSTICA LTDA" (Author "Priscila Sathler Garcia" "13.524/ - GO" "Engenheira" "Guava Engenharia")
        rteto = Region "Teto - Grid D" ("teto-grid-d" <> show (round soff)) "-164,103.83,88.66,0,0,5,20"
        dm = 0.25
        dr = 0.15
        db = 0.05
        bl = 21.85
        spl = 2.56
        bspl = 0.7
        sp = node (Sprinkler (Just (25,24.0))  (Just db) 14 (0.16*60*1.7) )
        te c dri dbi = node (Tee (TeeConfig (fst <$> c) (0.1*dbi) dbi dri (100)) Table )
        tubo d l = link [Tubo (Just d) l 100]
        pregrid = fst $ runInput $ mdo
          res <- node (Reservatorio 2 Nothing (Open 0))
          mainl <- link  (editDiametro dm <$> (principal <> [tubod 111.43 dm ,tubod 0.7 dm  , joelho, tubod 9.35 dm  ])) res to
          to <- te [mainl,lhid ,lvga] dr dr
          lhid <- tubo  dr 1 to hid
          hid <- node (Open 1900)
          lvga <- link (editDiametro dr <$> [jd (upC 0) dm , tubod 11 dm ,jd (dowC 0 ) dm ,tubod 0.8 dm, joelhoL , tubod 44.95 dm]) to t1
          t1 <-  te [l2,lvga,l5] dr dr
          l5 <- link  (editDiametro dr <$> [tubod 1.8 dr ,tubod 1.54 dr,joelhoR,tubod bl dr ,joelhoR,tubod 1.54 dr,tubod 3.59 dr ]) t1 t4
          t4 <- te  [l8,l7,l5] dr db
          l7 <- tubo    db bl t4 t5
          l8 <- tubo dr ldist t4 nij
          t5 <- te  [l2,l7,l9] dr db
          l2 <- tubo  dr 1.79 t5 t1
          l9 <- tubo  dr ldist  t5 nil
          let f = foldl1 (>~>) (replicate 11 (uncurry patch) <> replicate 4 (uncurry spkt ) )
          ((tr,tl),(nil,nij)) <- f  ((l9,l8),(or,ol))
          or <- node (Open 0)
          ol <- node (Open 0)
          return ()

        spkt (lir,lil) ~(nir,nil)  = mdo
          nr <- te [lir,ix,cr] dr db
          nl <- te [cl,ixl,lil] dr db
          ix <- tubo   db (soff*spl + bspl) nr ni
          ni <- sp
          let f = foldr1 (>=>) ((replicate snum ) spka)
              spka oldn  = mdo
                tubo db spl oldn  b
                b <- sp
                return b
          e <- f ni
          ixl <- tubo    db (bl - st * spl - bspl) nl   e
          cr<- tubo dr ldist nr nir
          cl <- tubo dr ldist nl nil
          return ((cr,cl),(nr,nl))

        patch (ti,tj) ~(i,j) = mdo
          idn2 <- te [idt3,idt1, tj] dr db
          idn1 <- te [ti,idt1,idt2] dr db
          idt1 <- tubo  db bl  idn2 idn1
          idt2 <- tubo  dr ldist i idn1
          idt3 <- tubo  dr ldist  j idn2
          return ((idt2,idt3),(idn1,idn2))

        tubod l d = Tubo (Just d) l 100
        snum =  2
        st =  soff  + snum
        ldist =  3.10
        principal = [tubod 0.1 dm ,joelhoD,tubod 0.5 dm,tubod 1 dm,joelhoU0,tubod 2.726 dm ,joelhoL,tubod  1.0 dm,bomba,tubod  1 dm,joelhoUV (-1/4),tubod  1 dm,joelhoD ,tubod  0.76 dm,joelhoL,tubod  3.72 dm,joelhoD,tubod 1 dm, joelhoU0,tubod  0.2 dm,joelhoU0,joelhoL , tubod  1  dm,joelhoD ,tubod 5 dm ,joelhoR , tubod  62.5  dm,joelhoL,tubod  80.0  dm,joelhoR ,tubod 3 dm , joelhoL ,tubod  26.76 dm, joelhoUV (-1/2), tubod 1.5 dm ,joelhoDV 0,joelho,tubod  8.43 dm]
          where
            bomba = Bomba (Just (930,7200)) bombaJohnson2 [] []
            dm = 0.25
            tubod l d = Tubo (Just d) l 100


johnson :: (Enum a,Show a,RealFloat a )=> [(ProjectHeader,Grid a)]
johnson =  conjb <> mezanino <>   (grid <$> [0..6])
  where
    ph = ProjectHeader  "Depósito Armazém Johnson & Johnson - Galpão 01"  "\"RODOVIA BR-153, QUADRA CH, JARDIM GUANABARA, GALPÃO 01, GOIÂNIA, GOIÁS\"" "ATLAS LOGÍSTICA LTDA" (Author "Priscila Sathler Garcia" "13.524/ - GO" "Engenheira" "Guava Engenharia")
    principal = [tubod 0.1 dm ,joelhoD,tubod 0.5 dm,tubod 1 dm,joelhoU0,tubod 2.726 dm ,joelhoL,tubod  1.0 dm,bomba,tubod  1 dm,joelhoUV (-1/4),tubod  1 dm,joelhoD ,tubod  0.76 dm,joelhoL,tubod  3.72 dm,joelhoD,tubod 1 dm, joelhoU0,tubod  0.2 dm,joelhoU0,joelhoL , tubod  1  dm,joelhoD ,tubod 5 dm ,joelhoR , tubod  62.5  dm,joelhoL,tubod  80.0  dm,joelhoR ,tubod 3 dm , joelhoL ,tubod  26.76 dm, joelhoUV (-1/2), tubod 1.5 dm ,joelhoDV 0,joelho,tubod  8.43 dm]
      where
        bomba = Bomba (Just (930,7200)) bombaJohnson2 [] []
        dm = 0.25
        tubod l d = Tubo (Just d) l 100
    grid  soff =  (ph rteto ,Grid [] links [] nodes)
      where
        rteto = Region "Teto - Grid D" ("teto-grid-d" <> show (round soff)) "-164,103.83,88.66,0,0,5,20"
        tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
        te i c dri dbi = (i,Tee (TeeConfig c (0.1*dbi) dbi dri (100)) Table )
        dm = 0.25
        dr = 0.15
        db = 0.05
        sp i = (i,Sprinkler (Just (25,24.0))  (Just db) 14 (0.16*60*1.7) )
        seqT (ti,tj) (idt,idn) n = concat $ fmap (seq (ti,tj) (idt,idn)) [0..n]
          where
            seq (ti,tj) (idt,idn) i = patchT (ti+ i*2 ,tj +i*2) (idt + i*3,idn + i*2)
        seqS (ti,tj) (idt,idn) n = concat $ fmap (seq (ti,tj) (idt,idn)) [0..n]
          where
            seq (ti,tj) (idt,idn) i = patchS (ti+ i*3 ,tj +i*3) (idt + i*3,idn + i*2)
        patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn + 2) (idn + 1) db bl , tubo (idt +2 )  i (idn +1)   dr ldist, tubo   (idt +3 )  j (idn +2)        dr ldist ]
        patchS (ti,tj)(idt,idn) = [te (idn +2) [ti,idt+1, idt+3 ] dr db, te (idn +1) [idt+2,idt+1,tj] dr db]
        bl = 21.85
        spl = 2.56
        bspl = 0.7
        nodes = [te 38 [1,100,101] dr dr
                ,(39,Open 1900)
                ,te 1 [2,101,5] dr dr
                ,te 4 [8,7,5] dr db
                ,te 5 [2,7,9] dr db]
                <> seqS (9,8) (9,5) 11
                <> [ te 30 [51,46,44] dr db
                , sp 101, sp 102, sp 103
                , te 31 [45,49,50] dr db]
                <> [ te 32 [56,52,51] dr db
                , sp 104, sp 105, sp 106
                , te 33 [50,55,57] dr db]
                <> [ te 34 [63,58,56] dr db
                , sp 107, sp 108, sp 109
                , te 35 [57,61,64] dr db]
                <> [ te 36 [70,65,63] dr db
                , sp 111, sp 112, sp 113
                , te 37 [64,68,70] dr db,(0,Reservatorio 2 Nothing (Open 0))]

        tubod l d = Tubo (Just d) l 100
        snum =  2
        spkrow ix l i e =  [tubo  (ix) i (head l) db (soff*spl + bspl) , tubo (ix + (length l ) )  (last l)   e db (bl - st * spl - bspl)] <> zipWith (\i (a,b) -> tubo i a b db spl ) [ix+1 ..] (zip (tail l) l )
        st =  soff  + snum
        ldist =  3.10
        links = [(1,0,38,editDiametro dm <$> (principal <> [tubod 111.43 dm ,tubod 0.7 dm  , joelho, tubod 9.35 dm  ]))
                ,(tubo 100 38 39 dr 1)
                ,(101 , 38 , 1 , editDiametro dr <$> [jd (upC 0) dm , tubod 11 dm ,jd (dowC 0 ) dm ,tubod 0.8 dm, joelhoL , tubod 44.95 dm])
                ,(5,1,4,editDiametro dr <$> [tubod 1.8 dr ,tubod 1.54 dr,joelhoR,tubod bl dr ,joelhoR,tubod 1.54 dr,tubod 3.59 dr ])
                ,tubo 2 5 1 dr 1.79
                ,tubo 7 4 5 db bl
                ,tubo 8 4 6 dr ldist
                ,tubo 9 5 7 dr ldist]
                <> seqT (8,9) (9,5) 11
                <> spkrow 46 [101..103] 30 31
                <> [tubo 51 30 32 dr ldist
                  ,tubo 50  31 33 dr ldist]
                <> spkrow 52 [104..106] 32 33
                <> [tubo 56 32 34 dr ldist
                ,tubo 57 33 35 dr ldist]
                <> spkrow 58 [107..109] 34 35
                <> [ tubo 63 34 36 dr ldist
                ,tubo 64 35 37 dr ldist ]
                <> spkrow 65  [111..113] 36 37

                <> [(70,37,36, editDiametro dr <$>  [tubod 1.42 dr,joelhoL ,tubod bl dr,joelhoL,tubod 1.42 dr])]
    vga3 rpav =
      let
        pru = 0.2
        top = (editDiametro pru <$> principal ) <> [joelhoL,tubod pru 0.4  ]
        vga3 = [tee TeRunR hidrante ([tubod pru 0.4 , joelhoUV 0 ,tubod pru 0.9, tubod pru 0.7 , joelhoDV 0 ,tubod pru 0.8 , joelhoUV 0 , tubod pru 7.74 ,tubod pru 0.3 ,joelhoDV 0] <> rpav )]
        hidrante = [tubod pru 1,Open 1900]
        tubod  d l  = Tubo (Just d) l 100
      in top <> vga3
    mezanino =
      let
          pru = 0.2
          dr = 0.15
          db = 0.04
          tubod  d l  = Tubo (Just d) l 100

          pav2 = Origem $  vga3 ([tubod pru 7.18,joelhoDV 0 , tubod pru 1.15 , joelhoUV (1/4)] <> rpav2)
          openL o l = [tee TeRunR end (o <> l)]
          rpav2 = [tubod pru 1,joelhoR, tubod pru 115]  <> (  replcomp 18 (openL [tubod dr 3.26]) $   [tee TeRunR rl [tubod dr 3.27,tee TeRunR rl [tubod dr 3.27, tee TeRunR rl [tubod dr 3.27 , tee TeRunR rl [tubod dr 3.27 ,tee TeRunR rl ([tubod dr 3.27,joelhoR]<> rl)]]] ]])
            where
              rl = [tubod db 1.0, sp,tubod db 3.5,sp] <> end
          sp = Sprinkler (Just (25,20.0)) Nothing 12.1  ( 0.10490627622503315*60)
          st = snd $ fst $ runState (unrollNode (0,Open 0) pav2) ((Open 0,0),(Open 0 ,0))
          grid st = Grid [] (fmap (\(l,h,t,e)-> (l,h,t, e)) $ snd st) [] (fst st <> [(0,Reservatorio 0 Nothing (Open 0))])
      in [(ph $ Region "Mezanino Superior" "mezanino-superior" "84,-56,-35,10,0,0,120",grid st)]
    conjb =
      let
          pru = 0.2
          dr = 0.08
          db = 0.025
          tubod  d l  = Tubo (Just d) l 100
          sp = Sprinkler (Just (25,11.5)) Nothing 12.1  ( 0.10490627622503315*60)
          pav2 = Origem $  vga3   rpav2
          cross o l = [tee TeRunL [tubod dr 0.01,tee TeRunR end (o <> l) ]end ]
          conj o l = [tee TeRunL  [tubod dr 0.01,tee TeRunR rr (o <> l)  ] rl ]
          rpav2 = [tubod pru 7.18,joelhoDV 0 , tubod pru 1.14 , joelhoUV (1/4),tubod dr 1] <> (replcomp 11 (cross [tubod dr 2.38]) $ cross [tubod dr 1 , sp, tubod dr 1.27,sp,tubod dr 0.1]  $   conj [tubod dr 1 ,sp ,tubod dr 1.27,sp,tubod dr 0.1] $ conj [] end)
            where
          rl = [tubod db 1.05,joelhoL,tubod db 0.20 ,sp] <> end
          rr = [tubod db 1.05,joelhoR,tubod db 0.20 ,sp] <> end
          st = snd $ fst $ runState (unrollNode (0,Open 0) pav2) ((Open 0,0),(Open 0 ,0))
          grid st = Grid [] (fmap (\(l,h,t,e)-> (l,h,t, e)) $ snd st) [] (fst st <> [(0,Reservatorio 0 Nothing (Open 0))])
      in [(ph $ Region "In Rack - Conjunto B" "in-rack-conjunto-b" "-96,-90,-11,0,0,0,120",grid st)]




testInputGrid = solveModel (gridInput 3)



terraAtacado =
  let
      pru = 0.125
      top = casaMaquina 0.125 (Bomba (Just (750,550/3*10)) bombaSF [] []) <> [joelhoL,tubod pru 1.56,joelhoL ,tubod pru 4.2 , joelhoUV 0 ,tubod pru 5.58,joelhoDV 0 ]
      pav2 = Origem $  top <> rpav2
      cross o l = [tee TeRunL end [tubod pru 0.01,tee TeRunR (o <> l) end ]]
      openL o l = [tee TeRunR end (o <> l)]
      rpav2 = [tubod pru 1,joelhoR, tubod pru 13.82,joelhoR , tubod pru 2.25  ]  <> ( cross  [tubod pru 4.04]  $ replcomp 5 (cross [tubod pru 3.27]) $ cross [tubod pru 1.54, joelhoL , tubod pru 1.28,joelhoR , tubod pru 1.59 ] $ replcomp 7 (openL [tubod pru 3.27]) $ openL [tubod pru 0.37, joelho45L,tubod 0.1 1.66 ,joelho45R ,tubod 0.1 1.81] $  replcomp 5 (openL [tubod 0.1 3.27]) $ openL [tubod 0.08 3.27] [tee TeRunR rl1 [tubod 0.08 3.27,tee TeRunR rl [tubod 0.08 3.27, tee TeRunR rl [tubod 0.065 3.27 ,tee TeRunR rl ([tubod 0.050 3.27,joelhoR]<> rl)]]] ])
        where
          rl = [tubod 0.05 4.9, tubod 0.04 7,sp ,tubod 0.032 3.5,sp,tubod 0.025 3.5 ,sp,tubod 0.025 3.5,sp] <> end
          rl1 = [tubod 0.05 4.9, tubod 0.04 7,tubod 0.032 3.5,tubod 0.025 3.5 ,sp,tubod 0.025 3.5,sp] <> end
      sp = Sprinkler (Just (13,8.0)) Nothing 12.1  ( 0.10490627622503315*60)
      st = snd $ fst $ runState (unrollNode (0,Open 0) pav2) ((Open 0,0),(Open 0 ,0))
      grid st = Grid [] (fmap (\(l,h,t,e)-> (l,h,t, e)) $ snd st) [] (fst st <> [(0,Reservatorio 0 Nothing (Open 0))])
  in [("VG1",grid st)]



(bouganvillePav2,bouganvilleSub2) =
  let
      top = [tubod 0.2 0.1 ,joelhoD,tubod 0.2 0.5,tubod 0.2 1,joelhoU0,tubod 0.2 2.726 ,joelhoL,tubod 0.2 1.0,Bomba (Just (600,1833)) bombaSF [] [],tubod 0.2 1,joelhoUV (-1/4),tubod 0.2 1,joelhoD ,tubod 0.2 0.76,joelhoL,tubod 0.2 3.72,joelhoD,tubod 0.2 1,joelhoU0,tubod 0.2 0.1,joelhoU0,tubod 0.2 1,joelhoD ,tubod 0.2 0.2,joelhoL,tubod 0.2 1.56,joelhoL ,tubod 0.2 4.2 ,joelho,tubod 0.2 23.584,joelhoL,tubod 0.2 50.026 ,joelho45R, tubod 0.2 2.94, joelhoDV (3/8) ,tubod 0.2 0.5 ]
      pav2 = Origem $  top <> [joelhoUV $ -1/4 ] <> rpav2
      rpav2 = [tubod 0.2 4.54, Turn $ 1/2 ,joelhoR ,  tubod 0.2 4.18,joelhoL , tubod 0.2 13.79,tubod 0.15 25.396,Turn $ 1/2 , joelhoL,tubod 0.125 13.775 ,tubod 0.1 22.354,tubod 0.065 7.793 , tee TeBranch rb lb ]
        where
          lb = [tubod 0.05 1.593 , tee TeRunL  [tubod  0.025 3.11, tee TeRunL  ([tubod 0.025 3.11 ,Turn $ 1/2 ,joelhoR] <>  l3) l2 ] l1 ]
          l1 = [tubod 0.032 1.325,sp,tubod 0.025 3.8,sp,tubod 0.025 3.8 ,sp] <> end
          l2 = [tubod 0.032 1.325,sp,tubod 0.025 3.8,sp] <> end
          l3 = [tubod 0.025 1.325,sp] <> end
          rb = [tubod 0.05 1.518,tee TeRunR l1 ([tubod 0.025 3.11,joelho] <>  l1)]
      sub2 = Origem $ top <> [ tubod 0.2  23.243 , joelhoUV $ 1/4 ,tubod 0.2 0.534,joelhoL,tubod 0.2 3.055,tubod 0.2 3.055 ,joelho45L,tubod 0.2 1.1 ,joelhoDV (-1/8),tubod 0.2 3.95 , joelhoUV $ 0 ] <> rsub2
      rsub2 = [tubod 0.2 29.45,tubod 0.125 2.953, Turn (-1/2) ,joelhoR, tubod 0.125 23.054,tubod 0.1 24.47, tubod 0.08 3.47, joelho45L , tubod 0.08 0.347,joelho45R,tubod 0.08 6.166,tee TeRunR r3 [tubod 0.065  3.5, tee TeRunR  r3 r2 ]]
        where
          r3 =  [tubod  0.05 8.332,tubod 0.04 6.85,sp,tubod 0.032 3.45,sp,tubod 0.025 4.19,sp, tubod 0.025 3.35,sp] <> end
          r2 =  [tubod 0.05 3.5,joelho,tubod  0.05 4.932,tubod 0.04 6.8,sp,tubod 0.032 3.45,sp,tubod 0.025 3.45,sp, tubod 0.025 1.94,sp] <> end
      sp = Sprinkler (Just (13,5.8)) Nothing 12.0 6.1
      tubod di d = Tubo (Just di) d 100
      st = snd $ fst $ runState (unrollNode (0,Open 0) pav2) ((Open 0,0),(Open 0 ,0))
      stsub  = snd $ fst $ runState (unrollNode (0,Open 0) sub2 ) ((Open 0,0),(Open 0 ,0))
      grid st = Grid [] (fmap (\(l,h,t,e)-> (l,h,t, e)) $ snd st) [] (fst st <> [(0,Reservatorio 0 Nothing (Open 0))])
  in (grid st
     ,grid stsub)


{-
 Change Named directions to rotation pair
 Connection Turns
(preangle,postangle)
90 - 1/4
45 - 1/8
-}

rightC d = (0,d)
right90  = rightC $ 1/4
right45  = rightC $ 1/8
leftC d = (0,-d)
left90   = leftC $ 1/4
left45   = leftC $ 1/8
upC r = (-1/4,-r)
dowC r = (1/4,r)

{-angleE (Joelho _ (_,_,c) DRight _ ) = (0,0,rd c )
angleE (Joelho _ (_,_,c) DLeft _ ) = (0,0,negate $ rd c)
angleE (Joelho _ (_,_,c) (DUp r) _ ) = (0,-1/4,r)
angleE (Joelho _ (_,_,c) (DDown r) _ ) = (0,1/4,r)-}


end = [tubod 0.025 0.01,Open 0]
tee b i j = Te  Nothing b i j
cruz i j k = tee TeRunL [tubod 0.05 0.01,tee TeRunR  k j ] i
tubod  d l  = Tubo (Just d) l 100

joelhoR  = Joelho Nothing ("Conexao","Joelho","90") right90  100
joelho  = joelhoR
joelhoL  = Joelho Nothing ("Conexao","Joelho","90") left90  100
joelho45  = joelho45R
joelho45R  = Joelho Nothing ("Conexao","Joelho","45") right45  100
joelho45L  = Joelho Nothing ("Conexao","Joelho","45") left45  100

joelhoU  = joelhoUV $ 1/4
joelhoU0  = joelhoUV 0
joelhoUV e = Joelho Nothing ("Conexao","Joelho","90") (upC e ) 100

joelhoD  = joelhoDV 0
joelhoD0  = joelhoDV $ 1/4
joelhoDV  c = Joelho Nothing ("Conexao","Joelho","90") (dowC  c ) 100



solveModel (header ,model) = do
  let
       iter = solveIter (makeIter 0 1 model ) jacobianEqNodeHeadGrid
       fname  = regionFile $ regionInfo header
  reportIter header 0 iter
  print $ "renderReport" <> (fname <> ".csv")
  callCommand $ "soffice --convert-to xls --infilter=csv:\"Text - txt - csv (StarCalc)\":59/44,34,0,1,,1033,true,true  " <> fname <> ".csv"
  print $ "renderReport" <> (fname <> ".xls")
  writeFile (  fname <> ".scad") $openSCAD (drawIter iter )
  print $ "renderSCAD" <> (fname <> ".scad")
  let command = "openscad -o " <> fname <> ".png " <> fname <> ".scad  --camera=" <> regionView (regionInfo header)
  putStrLn command
  callCommand  command
  -- diagramRender (name <>".svg") (drawIter model)
  -- print "renderDiagram"


testJohnson =  do
  mapM_ solveModel johnson

{-testTerraAtacado =  do
  mapM_ solveModel terraAtacado
-}


testBouganville =  do
  let
       iter = solveIter (makeIter 0 1 bouganvillePav2 ) jacobianEqNodeHeadGrid
       iter2 = solveIter (makeIter 0 1 bouganvilleSub2 ) jacobianEqNodeHeadGrid
  -- reportIter "bouganvillePav2" 0 iter
  -- reportIter "bouganvilleSub2" 0 iter2
  print "renderReport"
  writeFile "bouganville.scad" $openSCAD (drawIter iter  <> drawIter iter2)
  print "renderSCAD"
  diagramRender "bounganvillePav2.svg" (drawIter iter)
  diagramRender "bounganvilleSub2.svg" (drawIter iter2)
  print "renderDiagram"



testFokus =  do
  let
       preiter = (makeIter 0 2 fokus )
       iter = solveIter preiter jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  -- reportIter "fokus" 0 iter
  writeFile "fokus.scad" $openSCAD (drawIter iter)
  diagramRender "fokus.svg" (drawIter iter)


test4 =  do
  let
       iter = solveIter (makeIter 4 3 grid4) jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  printMatrix ( fst $ expandGrid iter)
  writeFile "circle.scad" $openSCAD (drawIter iter )

t5 =  do
  let
       preiter = (makeIter 0 1 westpoint)
       iter = solveIter preiter jacobianEqNodeHeadGrid
  -- print preiter
  {-printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  reportIter "tipo17" 0 iter-}
  writeFile "westpoint.scad" $openSCAD (drawIter preiter)


t3 =  do
  let
       iter = solveIter test3 jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  -- reportIter  "subsolo1" 212 iter
  writeFile "test3.scad" $openSCAD (drawIter iter )

sanIterSubsolo=  do
  let
       preiter = (makeIter 0 1 sanmarinoSubsolo)
       iter = solveIter preiter jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  -- reportIter "sanmarino_subsolo" 0 iter
  return (drawIter iter )


sanIterTerraco =  do
  let
       preiter = (makeIter 0 1 sanmarinoTerraco)
       iter = solveIter preiter jacobianEqNodeHeadGrid
  -- print preiter
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  -- reportIter "sanmarino_terraco" 0 iter
  return (drawIter iter )

sanmarino = do
  t1 <- sanIterTerraco
  t2 <- sanIterSubsolo
  writeFile "sanmarino.scad" $ openSCAD     (t1 <> t2)

main = testBouganville


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


nodesFlowSet g = findNodesLinks g $ fmap (\n@(ni,_) -> (ni,n)) $nodesFlow $ g

enableSprinklers d (Iteration n f g)  = Iteration n f (Grid  (linksPosition g) (links g) (shead g) (fmap nodes2  (nodesFlow g)) )
  where k =S.fromList $  activeNodes d
        nodes2 (i,s@(Sprinkler _ _ _ _)) =  (i,if S.member i k then s else Open 0)
        nodes2 i = i


upgradeGrid :: Int -> Int -> Grid Double -> Grid  Double
upgradeGrid ni li a = a {shead = M.toList nodesPos, linksPosition = M.toList linksPos}
  where
    (nodesPos,linksPos) =  snd $ runState (do
                      modify (<> (M.singleton ni (0,0), mempty))
                      locateGrid lmap nmap ni (0,0 ) (Left $ var li lmap ))
                        (mempty,mempty)
    lmap = M.fromList (fmap (\l@(li,_,_,_)-> (li,l))  $ links a)
    nmap = M.fromList (findNodesLinks a $ fmap (\n@(ni,_) -> (ni,n)) $ (nodesFlow a) )

recurse render r@(Right l@(ni,h,t,e)) = do
  lift $ modify (<> (S.empty,S.singleton ni))
  i <- fst <$> lift  get
  linkmap <- fst <$> ask
  let nexts = S.toList $ S.difference (S.fromList [h,t]) i
  ti <- mapM (recurse render . Left . flip var linkmap) nexts
  return $ render r  : concat ti
recurse render r@(Left n@(ni,lks,e)) = do
  lift $ modify (<>(S.singleton ni,S.empty))
  s <- snd <$> lift  get
  nodemap <- snd <$> ask
  let nexts = S.toList $ S.difference lks  s
  ti <- mapM (recurse render . Right . flip var nodemap) nexts
  return $ render r : concat ti

sf2 = formatFloatN 2
sf3 = formatFloatN 3

reportIter :: ProjectHeader -> Int -> Iteration Double -> IO ()
reportIter header i (Iteration f h a)  = do
    let name = regionFile $ regionInfo header
    writeFile (name <> ".csv")  $(headerCalculo <> "\n\n" <> "Relatório dos Nós" <> "\n\n" <>( L.intercalate "\n"    $ (L.intercalate ","  nodeHeader :) (evalState (runReaderT (do
           nodemap  <- fst <$> ask
           recurse (either nmap lmap) (Left $ fromJust $ M.lookup i nodemap )) (M.fromList $ fmap (\(i,(j,k))-> (i,(i,j,k))) $ findNodesLinks a (nodesFlow a) ,M.fromList $ fmap (\l@(i,h,t,e)-> (i,l))$ links a )) (S.empty,S.empty))) <>
            "\n\n" <> "Relatório dos Links" <> "\n\n" <> L.intercalate "," linkHeader <>"\n" <> (L.intercalate "\n" $ lsmap <$> (links a)))
  where
    vazao = fromJust $ M.lookup 1 (M.fromList f)
    bomba :: Element Double
    bomba@(Bomba (Just (pn,vn)) _ _  _) = fromJust $ join $ L.find isBomba . (\(_,_,_,l) -> l)<$> L.find (\(_,_,_,l) -> L.any isBomba l ) (links a )
    isBomba (Bomba _ _ _ _ ) = True
    isBomba _ = False
    isSprinkler  (Sprinkler _ _ _ _ ) = True
    isSprinkler _ = False
    pressao = abs $ pipeElement vazao bomba
    sprinklers =  fmap (\(i,e) -> (i,fromJust $ M.lookup i  (M.fromList h ),e)) $  L.filter (isSprinkler . snd) (nodesFlow a)
    (_,_,Sprinkler (Just (dsp,ksp))  _ _ _) = head sprinklers
    spkReport = L.intercalate "\n" $ L.intercalate ","  . (\(ix,p,e@(Sprinkler (Just (d,k )) (dl) f a ))-> [show ix , formatFloatN 2 p ,   formatFloatN 2 $ k*sqrt p]  ) <$>    sprinklers
    projectHeader =  L.intercalate "\n" ["Projeto," <> projectName header   ,"Proprietário," <> projectOwner header , "Endereço," <> endereco header, formation ainfo <> "," <> authorName ainfo , "Crea," <> creaNumber ainfo , "Empresa," <>   empresa ainfo , "Região Cálculo," <> regionName (regionInfo header)]
        where ainfo = authorInfo header
    headerCalculo = projectHeader <> "\n\n" <> "Dados do projeto" <> "\n\n" <> L.intercalate "\n\n"
            ["Bomba\n" <> "Pressão Nominal," <> sf2 pn <> ",kpa\n" <> "Vazão Nominal," <> sf2 vn <> ",L/min\n" <> "Potência,185,cv\n" <> "Vazão Operação," <> sf2 vazao <> ",L/min\n" <>  "Pressão Operação," <> sf2 pressao <> ",kpa,"
            ,"Reservatório\n" <> "Volume," <> sf2 (vazao * 60) <> ",L\n" <> "Tempo de Duração,60,min"
            ,"Sprinkler\nTipo,ESFR\n"  {-<> "Diâmetro,25,mm\n" <> "Area,9,m²\n" <> -}<> "K," <> sf2 ksp <> ",L/min/(kpa^(-1/2))\n"<> "Vazão Mínima," <> sf2 (ksp*sqrt 350) <>  ",L/min\n" <> "Pressão Mínima,350,kpa\n" <> "Quantidade Bicos," <> show (L.length sprinklers) <> ",un" ] <>  "\nId,Pressão(kpa),Vazão(L/min)\n" <> spkReport

    nmap = (\n@(ni,s,e) -> L.intercalate "," $ ["N-"<> show ni,formatFloatN 2 $ maybe 0 id $p ni , formatFloatN 2 $h  ni,"",""] ++  expandNode (p ni) e )
        where p ni =  varM ni hm
              h ni =  fst ( fromJust  (varM ni  pm)) ^. _z
    lmap = (\n@(ni,h,t,e) ->  L.intercalate "," ["T-"<> show h <>  "-" <> show t ,"","",formatFloatN 2 $  maybe 0 id (abs <$> p ni) ,formatFloatN 2  $ abs (pr h - pr t),"Trecho"])
        where pr ni =  maybe 0 id (varM ni hm)
              p ni =  varM ni fm
    lsmap = (\n@(ni,h,t,e) ->    (L.intercalate "\n" $ fmap (L.intercalate ",") $ expandLink ("T-"<> show h <>  "-" <> show t <> "-") (p ni) <$>  zip [0..] e))
        where p ni =  varM ni fm
    fm = M.fromList f
    hm = M.fromList h
    pm = M.fromList (shead a )
    nodeHeader = ["ID","Pressão Dinâmica (kpa)","Altura (m)","Vazão (L/min)","Perda (kpa)","Elemento"]
    expandNode (Just p) (Sprinkler (Just (d,k)) (dl) f a ) = ["Sprinkler"]
    expandNode _ (Reservatorio  _ _ _)  = ["Reservatorio"]
    expandNode _ (Tee (TeeConfig i r db dr c) _ )  = ["Tê"]
    expandNode _ (Open 0 )  = ["Tampa"]
    expandNode _ (Open i )  = ["Hidrante"]
    expandNode _ i = []
    linkHeader = ["SubTrecho","Diametro (m)","Perda (kpa)","Elemento","Comprimento (m)"]
    expandLink st (Just f) (i,t@(Tubo (Just d ) dl  _)) = [st <> show i ,  sf3 d , sf2$   Grid.ktubo t*(abs f)**1.85,"Tubo " , sf3 dl ]
    expandLink st (Just f) (i,t@(Turn   _)) = [st <> show i ,  "" , "","Turn" , "" ]
    expandLink st (Just f) (i,b@(Bomba (Just (d,_))  dl  _ _ )) = [st <> show i, "", sf2 $ pipeElement f b,"Bomba"]
    expandLink st (Just f) (i,j@(Joelho (Just d)  (_,_,c)  _ _ ) ) = [st <> show i , sf3 d, sf2 $Grid.ktubo j*(abs f)**1.85,"Joelho " <> c]




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
    nodePressures = M.fromList $ nodeHeads a <> (fmap (\(j,i) -> (j,9.81* ( fst i ^. _z))) $ shead (grid a))
    lookNode i = (i,var i nodeMap)
    linkflow = M.fromList (flows a)
    sortedHeads = L.sortBy (flip (comparing (\(i,p) ->  totalHead 0.08 p ((/2) $ sum $ fmap (abs .snd ) $ S.toList $ fst $ var i nodeMap))))  (nodeHeads a)
    nodeMap =  fmap (\(s,i) -> (S.map (\si-> (si,var si linkflow)) s, i) ) $ M.fromList $findNodesLinks (grid a) $ (fmap (Left ) <$>  (shead $ grid a )) <> (fmap Right <$> (nodesFlow $ grid a))
    linkMap = M.fromList $ (\l@(i,_,_,_) -> (i,l)) <$> (links $ grid a)

findNodesLinks grid = fmap (\(i,n) -> (i,(var i nodeMapSet,n)))
    where nodeMapSet = fmap S.fromList $ M.fromListWith mappend $ concat $ (\(l,h,t,_) -> [(h,[l ]),(t,[l ])]) <$> links grid
