{-# LANGUAGE TupleSections,NoMonomorphismRestriction #-}
module Tee where

import Debug.Trace
import Data.Monoid
import Element
import qualified Data.Map as M
import Data.Maybe
import GHC.Stack
--
-- From Pipe Flow A Pratical and Comprehensive Guide - Chapter Tee - page 170 to 199
--

eps = 1e-12

-- g =  32.174
g =  9.81

testTee = [(conf,classifyTee  Table flows conf),(conf,classifyTee Formula flows conf)]
  where conf = TeeConfig  config 0.09 0.065 0.065 1000
        config = [1,2,3]
        flows = fmap (/(1000*60))(M.fromList [(1,0),(2,-1166 ),(3,500)])

allMaybes i
  | all isJust i = fmap fromJust i
  | otherwise = []

fromJustE e (Just i) = i
fromJustE e i = errorWithStackTrace $ "fromJustE" <> e

ktubo t  v = perda*10*v**1.85
        where
              (Just d ) = diametroE t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))


classifyTee  Formula flowMap  t@(TeeConfig _ _ _ _ _) =  fmap (/1000) <$> classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        [rli,bi,rri] = teeConfig t
        db = teeDiameterBranch t
        dr = teeDiameterRun t
        r =  teeRadius t
        rho = teeMaterial t
        classifyFlow bl@[rls,bs,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 = zip [rri,bi] $(\f -> f db dr dr rho r) <$> [divergingFlowThroughRun b rr rl ,divergingFlowThroughBranch b rr rl ]
          |  rrs > 0 && bs <= 0 && rls <= 0 = zip [rli,bi] $(\f -> f db dr dr rho r) <$> [divergingFlowThroughRun b rl rr ,divergingFlowThroughBranch b rl rr ]
          |  rls >= 0 && bs >= 0 && rrs < 0 = zip [rli,bi] $(\f -> f db dr dr rho r) <$> [convergentFlowThroughRun b rl rr ,convergentFlowThroughBranch b rl rr ]
          |  rrs >= 0 && bs >= 0 && rls < 0 = zip [rri,bi] $(\f -> f db dr dr rho r) <$> [convergentFlowThroughRun b rr rl ,convergentFlowThroughBranch b rr rl ]
          |  bs > 0 && rrs <= 0 && rls <= 0 = allMaybes $ zipWith (\i->fmap (i,)) [rli,rri] $fmap (either (\ e -> traceShow (e ++ show t) Nothing ) (Just ) ) $(\f -> f dr dr db rho r) <$>[divergingFlowFromBranch rl rr b ,divergingFlowFromBranch rr rl b ]
          |  bs < 0 && rrs >= 0 && rls >= 0 = allMaybes $ zipWith (\i->fmap (i,)) [rli,rri] $ fmap (either (\ e -> traceShow (e ++ show t) Nothing ) (Just )) $(\f -> f dr dr db rho r) <$>[convergentFlowIntoBranch rl rr b ,convergentFlowIntoBranch rr rl b ]
          | otherwise =  traceShow ("no case for branch list " ++ show  t ++ show flow ++ show flowMap) []
          where rl = abs rls
                rr = abs rrs
                b = abs bs

classifyTee  Table flowMap  t =  classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        [rli,bi,rri] = teeConfig t
        db = teeDiameterBranch t
        dr = teeDiameterRun t
        -- r =  teeRadius t
        rho = teeMaterial t
        direct = Perda (Just $ dr) ("Conexao","Te","Direta")  rho
        lateral = Perda (Just $db) ("Conexao","Te","Lateral")  rho
        classifyFlow bl@[rls,bs,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 = zip [rri,bi]  [ktubo direct rr,ktubo lateral b]
          |  rrs > 0 && bs <= 0 && rls <= 0 = zip [rli,bi]  [ktubo direct rl,ktubo lateral b]
          |  rls >= 0 && bs >= 0 && rrs < 0 = zip [rli,bi]  [ktubo direct rl,ktubo lateral b]
          |  rrs >= 0 && bs >= 0 && rls < 0 = zip [rri,bi]  [ktubo direct rr,ktubo lateral b]
          |  bs > 0 && rrs <= 0 && rls <= 0 = zip [rli,rri] [ktubo lateral rl,ktubo lateral rr]
          |  bs < 0 && rrs >= 0 && rls >= 0 = zip [rli,rri] [ktubo lateral rl,ktubo lateral rr]
          | otherwise =  traceShow ("no case for branch list " ++ show  t ++ show flow ++ show flowMap) []
          where rl = abs rls
                rr = abs rrs
                b = abs bs

classifyTeeEl  Table flowMap  t =  classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        [rli,bi,rri] = teeConfig t
        db = teeDiameterBranch t
        dr = teeDiameterRun t
        -- r =  teeRadius t
        rho = teeMaterial t
        direct = Perda (Just $ dr) ("Conexao","Te","Direta")  rho
        lateral = Perda (Just $db) ("Conexao","Te","Lateral")  rho
        classifyFlow bl@[rls,bs,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 = zip [rri,bi]  [direct,lateral]
          |  rrs > 0 && bs <= 0 && rls <= 0 = zip [rli,bi]  [direct ,lateral]
          |  rls >= 0 && bs >= 0 && rrs < 0 = zip [rli,bi]  [direct ,lateral]
          |  rrs >= 0 && bs >= 0 && rls < 0 = zip [rri,bi]  [direct ,lateral]
          |  bs > 0 && rrs <= 0 && rls <= 0 = zip [rli,rri] [lateral ,lateral]
          |  bs < 0 && rrs >= 0 && rls >= 0 = zip [rli,rri] [lateral ,lateral]
          | otherwise =  traceShow ("no case for branch list " ++ show  t ++ show flow ++ show flowMap) []
          where -- rl = abs rls
                -- rr = abs rrs
                -- b = abs bs


divergingFlowThroughRun :: (Ord a,Floating a) => a -> a -> a -> a -> a -> a -> a -> a -> a
divergingFlowThroughRun  w3 0 w1  d3 d2 d1 rho  r  = 0
divergingFlowThroughRun  w3 w2 w1  d3 d2 d1 rho  r = (1.62 - 0.98* w1/w2 -  0.64*(w1/w2)^2 + 0.03*(w2/w1)^2 )*rho*w2^2/(2*a1^2)
  where a1 = area d1

divergingFlowThroughBranch :: (Ord a,Floating a) => a -> a -> a -> a -> a -> a -> a -> a -> a
divergingFlowThroughBranch 0 w2 w1 d3 d2 d1 rho r = 0
divergingFlowThroughBranch w3 w2 w1 d3 d2 d1 rho r = rho*w3**2/(2*a3^2) *(0.81 - 1.13*w1/w3)*d3^4/d1^4 +1.00 + 1.12*d3/d1 - 1.08*d3^3/d1^3 + k93 r d3
  where k93 r d3 = 0.57 - 1.07*sqrt (r/d3) - 2.13*(r/d3) + 8.24*(r/d3)**1.5 - 8.48*(r/d3)^2 +2.9*(r/d3)**2.5
        a3 = area d3

divergingFlowFromBranch :: (Ord a ,Floating a) => a -> a -> a -> a -> a -> a -> a -> a -> Either String a
divergingFlowFromBranch w3 0 w1 d3 d2 d1 rho r = Right 0
divergingFlowFromBranch w3 w2 w1 d3 d2 d1 rho r
  | d1 /= d2  = Left "No formula for divergingFlowFromBranch with non unity branches"
  | w1/w2 >= 1/0.2 && w1/w2 <= 1/0.8= Left  "No formula for divergingFlowFromBranch outsie w2/w1 >=0.2 && w2/w1 <= 0.8"
  | otherwise  =  Right $ w2^2*rho/(2*a2^2)* (1.59*w1^2/w2^2 + (1.18 - 1.84*sqrt(r/d) + 1.16*r/d)*w1/w2 - 1.68 + 1.04*sqrt(r/d) - 1.16*r/d)
    where a2 = area d2
          d = d1

cm :: Floating a => a -> a -> a
cm r d3 = 0.23 + 1.46*(r/d3) - 2.75*(r/d3)**2 + 1.83*(r/d3)**3
cxc :: Floating a => a -> a -> a
cxc r d3 = 0.08 + 0.56*(r/d3) - 1.75*(r/d3)**2 + 1.83*(r/d3)**3
cyc :: Floating a => a -> a -> a -> a
cyc r d1 d3 = 1 - 0.25*(d3/d1)**1.3 - (0.11 * r /d3 - 0.65*r**2/d3**2 + 0.83*r**3/d3**3)*d3**2/d1**2

convergentFlowThroughRun :: (Ord a,Floating a) => a -> a -> a -> a -> a -> a -> a -> a -> a
convergentFlowThroughRun  w3 0 w1 d3 d2 d1 rho r = 0
convergentFlowThroughRun  w3 w2 w1 d3 d2 d1 rho r = rho*w2**2/(2*a1**2)*(2*w1**2/w2**2 - 1.95 - 2*cxc r d3 *(w1/w2 -1) - 2*cm r d3 *(w1**2/w2**2 - w1/w2))
  where
        a1 = area d1

convergentFlowThroughBranch :: (Ord a,Floating a) => a -> a -> a -> a -> a -> a -> a -> a -> a
convergentFlowThroughBranch 0 w2 w1 d3 d2 d1 rho r = 0
convergentFlowThroughBranch w3 w2 w1 d3 d2 d1 rho r = rho*w3**2/(2*a3**2)*((2*cyc r d1 d3 -1) + d3**4/d1**4*(2*(cxc r d3 - 1) + 2*(2 - cxc r d3 - cm  r d3 )*w1/w3 + 0.08*w1**2/w3**2))
  where a3 = area d3

convergentFlowIntoBranch :: (Eq a,Floating a ) => a -> a -> a -> a -> a -> a -> a -> a -> Either String a
convergentFlowIntoBranch  w3 0 w1 d3 d2 d1 rho r = Right 0
convergentFlowIntoBranch  w3 w2 w1 d3 d2 d1 rho r
  | d1 /= d2  = Left "No formula for convergentFlowIntoBranch with non unity branches"
  | otherwise  =  Right $ rho*w2**2/(2*a2^2)* ((1.18 - 1.16*sqrt(r/d) + 0.5*r/d)*w1^2/w2^2 - (0.95 - 1.65*r/d)*w1/w2 + 0.34 -1.69*r/d)
    where a2 = area d2
          d = d1

area :: Floating a =>  a -> a
area d = pi*(d/2)**2

