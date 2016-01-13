{-# LANGUAGE TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Grid
import Debug.Trace
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

import Diagrams.Prelude hiding (end)


path i h t  l = (i,h,t,  l)

tubod l d = Tubo (Just d) l 100
jd dir d = Joelho (Just d) ("Conexao","Joelho","90")  dir 100

makeIter i j g = Iteration ( zip (fmap (\(i,_,_,_)-> i) (links g)) (( replicate 10 (20) <> repeat 4) )) ( zip ( fmap fst $ filter (not .isReservoir.snd)  (nodesFlow g))(repeat 100)) (realToFrac  <$>  upgradeGrid i j g)


grid4 :: RealFloat a => Grid a
grid4 =  Grid [] links [] nodes
  where nodes = [te 3 [1,3,2] 0.065 0.065  ,(4,Open 0 ),(1,Reservatorio 0 Nothing (Open 0)),(2,Reservatorio 0 Nothing (Open 0))]
        links = [(1,3,2,($0.065) <$> [tubod 0.2,jd $ upC 0 ,tubod 0.4])
                ,(2,3,1,($0.065) <$> [tubod 0.2,jd $ dowC 0,tubod 0.4])
                ,(3,4,3,($0.065) <$> [tubod 2,jd $ right90 ,tubod 2])]
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (100)))

fokus :: RealFloat a => Grid a
fokus = (Grid [] links [] nodes)
  where
    te i c dri dbi =  (i,Tee (StaticTee c (0.1*dbi) dbi dri (100)))
    tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
    dm = 0.20
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
            {-,te 4 [8,7,6] dr db
            ,te 5 [1,7,9] dr db-}]
            <> seqS (1,6) (9,5) 11
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

    links = [(2,0,1,editDiametro dm <$> [tubod 0.1 dm ,joelhoD,tubod 0.5 dm,tubod 1 dm,joelhoU0,tubod 2.726 dm ,joelhoL,tubod  1.0 dm,bomba,tubod  1 dm,joelhoUV (-1/4),tubod  1 dm,joelhoD ,tubod  0.76 dm,joelhoL,tubod  3.72 dm,joelhoD,tubod 1 dm, joelhoU0,tubod  0.2 dm,joelhoU0,joelhoL , tubod  1  dm,joelhoD ,tubod 3.6 dm ,joelhoL , tubod  22.3 dm,joelho,tubod  21.66  dm,joelhoL ,tubod 12.75 dm , tubod  62.46   dm,joelho,tubod  0.768 dm,jd (upC 0) dm , tubod 11 dm ,jd (dowC 0 ) dm ,tubod ({-20*2.89+-}2.889) dm,tubod 0.1 dr])
            ,tubo 3 1 2 dr 1.8
            ,tubo 4 2 3 db 49.26
            ,(5,2,3,editDiametro dr <$> [tubod 1.54 dr,joelhoR,tubod 49.26 dr ,joelhoR,tubod 1.54 dr])
            ,tubo 6 3 6 dr 3.59
            ,tubo 1 7 1 dr 1.79
            {-,tubo 7 4 5 db 49.26
            ,tubo 8 4 6 dr 3.7
            ,tubo 9 5 7 dr 3.7-}]
            <> seqT (6,1) (9,5) 11
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

fokus2 :: (Show a ,Ord a,RealFloat a )=> Iteration a
fokus2 = Iteration ( zip (fmap (\(i,_,_,_)-> i) links) (repeat 4 )) ( zip ( fmap fst $ filter (not .isReservoir.snd)  nodes )   (repeat 100) ) (realToFrac  <$>  upgradeGrid 212 31 grid)
  where
        grid =  (Grid  [] ((\(i,j,k,l) -> (i,j,k,fmap (editDiametro 0.8) l ))  <$> links) [] nodes )
        sp i = (i,Sprinkler (Just (16,15.1))  Nothing 12 6.1)
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (100)))
        tubo i h t d l = (i,h,t,[Tubo (Just d) l 100])
        bomba = Bomba (Just (300,1166)) bombaSF [] []
        patchT (i,j) (idt,idn) = [tubo (idt + 1) (idn + 2) (idn + 1) 0.025 (1.4 + 4*2.92) , tubo (idt +2 )  i (idn +1)   0.065 2.25, tubo   (idt +3 )  j (idn +2)        0.08 (2.25)]
        patchS (idt,idn) (ti,tj)= [te (idn +2) [idt+3,idt+1,ti ] 0.08 0.025, te (idn +1) [tj,idt+1,idt +2] 0.065 0.025]

        nodes = [ te 240 [77,31,74] 0.08 0.1]
                <> [(238,Open 4),(239,Open 4),(212,Reservatorio 0 Nothing (Open 0))]

        links = [(31,212,240 , [tubod 0.1 0.1 ,joelhoD,tubod 0.5 0.1,tubod 1 0.1,joelhoU0,tubod 2.726 0.1 ,joelhoL,tubod  1.0 0.1,bomba,tubod  1 0.1,joelhoUV $ -1/4 ,  tubod  1 0.1,joelhoD ,tubod  0.76 0.1,joelhoL,tubod  3.72 0.1,joelhoD,tubod 1 0.1, joelhoU0,tubod  0.2 0.1,joelhoU0,tubod  1 0.1,joelhoD ,tubod  0.2 0.1,joelhoL,tubod  1.56 0.1,joelhoL ,tubod  4.2 0.1,joelho,tubod  0.768 0.1,jd (dowC 0) 0.1, tubod (4*3.24) 0.1,tubod (11*3.06) 0.1,tubod 3.06 0.1, tubod 3.06 0.1 ,tubod 2.88 0.1 ,jd (upC $ 1/2) 0.1 ,tubod ({-20*2.89+-}2.889) 0.10 ])]
                <> [tubo 74 239 240 0.08 2,path 77 240 238 [tubod (1.0) 0.08 ,tubod 5.57 0.065 ,tubod 12.3674 0.065,jd left90 0.065, tubod (1.507) 0.065]]



test3 :: (Show a ,Ord a,Fractional a )=> Iteration a
test3 = Iteration ( zip (fmap (\(i,_,_,_)-> i) links) (repeat 4 )) ( zip ( fmap fst $ filter (not .isReservoir.snd)  nodes )   (repeat 100) ) (realToFrac  <$>  upgradeGrid 212 31 grid)
  where
        grid = (Grid  [] links [] nodes )
        sp i = (i,Sprinkler (Just (0.013,8))  (Just 0.025) 12 6.1)
        tubo' i h  d l = (i,h,h+1,[Tubo (Just d) l 100])
        te i c dr db =  (i,Tee (TeeConfig c (0.1*db) db dr (100)))
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

sanmarinoSubsolo =
  let r1 = Origem $  [tubod 0.1 0.1 ,joelhoD,tubod 0.1 0.5,tubod 0.1 1,joelhoU0,tubod 0.1 2.726 ,joelhoL,tubod 0.1 1.0,Bomba (Just (300,1566)) bombaSF [] [],tubod 0.1 1,joelhoUV (-1/4),tubod 0.1 1,joelhoD ,tubod 0.1 0.76,joelhoL,tubod 0.1 3.72,joelhoD,tubod 0.1 1,joelhoU0,tubod 0.1 0.2,joelhoU0,tubod 0.1 1,joelhoD ,tubod 0.1 0.2,joelhoL,tubod 0.1 1.56,joelhoL ,tubod 0.1 4.2 ,joelho,tubod 0.1 0.768 ,joelhoD0,tubod 0.1  (20*3.24 + 3.04) ,joelhoU] <> r45
      r45 = [tubod 0.065 0.1 , hid , tubod 0.065 0.1 , hid,tubod 0.065 6.15 , tee TeRunR r3 r23]
      r23 = [tubod 0.065 4.00 , tee TeRunR r3 r2]
      r2 = [tubod 0.065 4,joelho, tubod 0.04 8.9413 , tee TeRunL [  tubod 0.025 0.4597  ,sp ,tubod 0.025 3.54, tee TeRunL  [tubod 0.025 0.4587 ,sp , tubod 0.025 0.01 ,Open 0 ][tubod 0.025 2.36  ,joelho45 , tubod 0.025 0.5402 ,sp,tubod 0.025 0.1,Open 0]] [tubod 0.025 1.96 ,joelho45 , tubod 0.025 0.5402 ,sp,tubod 0.025 0.1,Open 0] ]
      -- r2 = [tubod 0.04 9.40,sp,tubod 0.032 4.00 ,sp,tubod 0.025 4.00 ,sp ,tubod 0.025 3.00 ,sp,tubod 0.025 0.1,Open 0]
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


(bouganvillePav2,bouganvilleSub2) =
  let
      top = [tubod 0.2 0.1 ,joelhoD,tubod 0.2 0.5,tubod 0.2 1,joelhoU0,tubod 0.2 2.726 ,joelhoL,tubod 0.2 1.0,Bomba (Just (600,1166)) bombaSF [] [],tubod 0.2 1,joelhoUV (-1/4),tubod 0.2 1,joelhoD ,tubod 0.2 0.76,joelhoL,tubod 0.2 3.72,joelhoD,tubod 0.2 1,joelhoU0,tubod 0.2 0.1,joelhoU0,tubod 0.2 1,joelhoD ,tubod 0.2 0.2,joelhoL,tubod 0.2 1.56,joelhoL ,tubod 0.2 4.2 ,joelho,tubod 0.2 23.584,joelhoL,tubod 0.2 50.026 ,joelho45R, tubod 0.2 2.94, joelhoDV (3/8) ,tubod 0.2 0.5 ]
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



testBouganville =  do
  let
       iter = solveIter (makeIter 0 1 bouganvillePav2 ) jacobianEqNodeHeadGrid
       iter2 = solveIter (makeIter 0 1 bouganvilleSub2 ) jacobianEqNodeHeadGrid
  reportIter "bouganvillePav2" 0 iter
  reportIter "bouganvilleSub2" 0 iter2
  print "renderReport"
  writeFile "bouganville.scad" $openSCAD (drawIter iter  <> drawIter iter2)
  print "renderSCAD"
  diagramRender "bounganvillePav2.svg" (drawIter iter)
  diagramRender "bounganvilleSub2.svg" (drawIter iter2)
  print "renderDiagram"



testFokus =  do
  let
       preiter = (makeIter 0 2 fokus )
 {-      iter = solveIter preiter jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  reportIter "fokus" 0 iter-}
  writeFile "fokus.scad" $openSCAD (drawIter preiter)
  diagramRender "fokus.svg" (drawIter preiter)


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
  reportIter  "subsolo1" 212 iter
  writeFile "test3.scad" $openSCAD (drawIter iter )

sanIterSubsolo=  do
  let
       preiter = (makeIter 0 1 sanmarinoSubsolo)
       iter = solveIter preiter jacobianEqNodeHeadGrid
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  reportIter "sanmarino_subsolo" 0 iter
  return (drawIter iter )


sanIterTerraco =  do
  let
       preiter = (makeIter 0 1 sanmarinoTerraco)
       iter = solveIter preiter jacobianEqNodeHeadGrid
  -- print preiter
  printMatrix $ lintInitialConditions iter
  printMatrix $ lintGridElements (grid iter)
  reportIter "sanmarino_terraco" 0 iter
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

reportIter :: String -> Int -> Iteration Double -> IO ()
reportIter name i (Iteration f h a)  = do
    writeFile (name <> ".csv")  $(( L.intercalate "\n"    $ (L.intercalate ","  nodeHeader :) (evalState (runReaderT (do
           nodemap  <- fst <$> ask
           recurse (either nmap lmap) (Left $ fromJust $ M.lookup i nodemap )) (M.fromList $ fmap (\(i,(j,k))-> (i,(i,j,k))) $ findNodesLinks a (nodesFlow a) ,M.fromList $ fmap (\l@(i,h,t,e)-> (i,l))$ links a )) (S.empty,S.empty)))
            <> "\n\n" <> L.intercalate "," linkHeader <>"\n" <> (L.intercalate "\n" $ lsmap <$> (links a))
            <> "\n\n" <> "Bomba\n" <> "Pressão Nominal;250;kpa\n" <> "Vazão Nominal;1166;L/min\n" <> "Potência;10;cv"
            <> "\n\n" <> "Reservatório\n" <> "Volume;60000;L\n" <> "Tempo de Duração;30;min"
            <> "\n\n" <> "Sprinkler\n" <> "Diâmetro;11;mm\n" <> "Area;12;m²\n" <> "K;8;L/min/(kpa^(-1/2))\n"<> "Vazão Mínima;54;L/min" )
  where
    nmap = (\n@(ni,s,e) -> L.intercalate "," $ ["N-"<> show ni,show $ maybe 0 id $p ni , show $h  ni,"",""] ++  expandNode (p ni) e )
        where p ni =  varM ni hm
              h ni =  fst ( fromJust  (varM ni  pm)) ^. _z
    lmap = (\n@(ni,h,t,e) ->  L.intercalate "," ["T-"<> show h <>  "-" <> show t ,"","",show $ maybe 0 id (abs <$> p ni) ,show $ abs (pr h - pr t){-  show  (sum $ expandLink' ("L-"<> show h <>  "-" <> show t <> "-") (p ni) <$>  zip [0..] e)-},"Trecho"])
        where pr ni =  maybe 0 id (varM ni hm)
              p ni =  varM ni fm
    lsmap = (\n@(ni,h,t,e) ->    (L.intercalate "\n" $ fmap (L.intercalate ",") $ expandLink ("T-"<> show h <>  "-" <> show t <> "-") (p ni) <$>  zip [0..] e))
        where p ni =  varM ni fm
    fm = M.fromList f
    hm = M.fromList h
    pm = M.fromList (shead a )
    nodeHeader = ["ID","Pressão Dinâmica (kpa)","Altura (m)","Vazão (L/min)","Perda (kpa)","Elemento","Vazão Chuveiro"]
    expandNode (Just p) (Sprinkler (Just (d,k)) (dl) f a ) = ["Sprinkler", show $ k*sqrt p]
    expandNode _ (Reservatorio  _ _ _)  = ["Reservatorio"]
    expandNode _ (Tee (TeeConfig i r db dr c) )  = ["Tê"]
    expandNode _ (Tee (StaticTee i r db dr c) )  = ["Tê"]
    expandNode _ (Open 0 )  = ["Tampa"]
    expandNode _ i = []
    linkHeader = ["SubTrecho","Diametro (m)","Perda (kpa)","Elemento","Comprimento (m)"]
    expandLink st (Just f) (i,t@(Tubo (Just d ) dl  _)) = [st <> show i ,  show d , show $ Grid.ktubo t*(abs f)**1.85,"Tubo " , formatFloatN 2 dl ]
    expandLink st (Just f) (i,t@(Turn   _)) = [st <> show i ,  "" , "","Turn" , "" ]
    expandLink st (Just f) (i,b@(Bomba (Just (d,_))  dl  _ _ )) = [st <> show i, "0.1 - 0.1", show $ pipeElement f b,"Bomba"]
    expandLink st (Just f) (i,j@(Joelho (Just d)  (_,_,c)  _ _ ) ) = [st <> show i , show d, show $Grid.ktubo j*(abs f)**1.85,"Joelho " <> c]

    expandLink' st (Just f) (i,t@(Tubo (Just d ) dl  _)) = Grid.ktubo t*(abs f)**1.85
    expandLink' st (Just f) (i,t@(Turn   _)) = 0 -- Grid.ktubo t*(abs f)**1.85
    expandLink' st (Just f) (i,b@(Bomba (Just (d,_))  dl  _ _ )) =  pipeElement f b
    expandLink' st (Just f) (i,j@(Joelho (Just d)  (_,_,c)  _ _ ) ) =  Grid.ktubo j*(abs f)**1.85



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
