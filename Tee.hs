{-# LANGUAGE NoMonomorphismRestriction #-}
module Tee where

import Debug.Trace
import qualified Data.Map as M
import Control.Applicative
import Data.Maybe
--
-- From Pipe Flow A Pratical and Comprehensive Guide - Chapter Tee - page 170 to 199
--

eps = 1e-12

-- g =  32.174
g =  9.81

testTee = (conf,classifyTee (M.fromList [(1,-616),(2,1116),(3,-500)]) conf)
  where conf = (TeeConfig [1,2,3] 0.01 0.65 0.65 (9.81*1000))
allMaybes i
  | all isJust i = fmap fromJust i
  | otherwise = []

fromJustE e (Just i) = i
fromJustE e i = error e
classifyTee  flowMap  t =  classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        [rli,bi,rri] = teeConfig t
        db = teeDiameterBranch t
        dr = teeDiameterRun t
        r =  teeRadius t
        rho = teeMaterial t
        classifyFlow bl@[rls,bs,rrs]
          |  rls > 0 && bs < 0 && rrs < 0 =  zip [rri,bi] $ (\f -> f g rho r) <$> [divergingFlowThroughRun b rr rl db dr dr,divergingFlowThroughBranch b rr rl db dr dr]
          |  rrs > 0 && bs < 0 && rls < 0 = zip [rli,bi] $(\f -> f g rho r) <$>  [divergingFlowThroughRun b rl rr db dr dr,divergingFlowThroughBranch b rl rr db dr dr]
          |  rls > 0 && bs > 0 && rrs < 0 = zip [rli,bi] $(\f -> f g rho r) <$>[convergentFlowThroughRun b rl rr db dr dr,convergentFlowThroughBranch b rl rr db dr  dr]
          |  rrs > 0 && bs > 0 && rls < 0 =  zip [rri,bi] $(\f -> f g rho r) <$>[convergentFlowThroughRun b rr rl db dr dr,convergentFlowThroughBranch b rr rl db dr dr]
          |  bs > 0 && rrs < 0 && rls < 0 =   zip [rli,rri] $ allMaybes $ fmap (either (\ e -> traceShow (e ++ show t) Nothing ) (Just ) ) $(\f -> f g rho r) <$>[divergingFlowFromBranch rl rr b dr dr db,divergingFlowFromBranch rr rl b dr dr db]
          |  bs < 0 && rrs > 0 && rls > 0 =  zip [rli,rri] $ allMaybes $ fmap (either (\ e -> traceShow (e ++ show t) Nothing ) (Just )) $(\f -> f g rho r) <$>[convergentFlowIntoBranch rl rr b dr dr db,convergentFlowIntoBranch rr rl b dr dr db]
          | otherwise =  [] -- error $ "no case for branch list " ++ show  t ++ show flow ++ show flowMap
          where rl = abs rls
                rr = abs rrs
                b = abs bs

data TeeConfig a
  = TeeConfig
  { teeConfig :: [Int] -- Left Run Branch Right Run
  , teeRadius :: a
  , teeDiameterBranch :: a
  , teeDiameterRun :: a
  , teeMaterial :: a
  }deriving(Eq,Ord,Show)


divergingFlowThroughRun :: Floating a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
divergingFlowThroughRun  w3 w2 w1  d3 d2 d1 g rho  r =  (1.62 - 0.98* w1/w2 -  0.64*(w1/w2)^2 + 0.03*(w2/w1)^2 ) * w2^2/(2*g*rho*a1^2)
  where a1 = area d1

divergingFlowThroughBranch :: Floating a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
divergingFlowThroughBranch w3 w2 w1 d3 d2 d1 g rho r = w3**2/(2*g*rho*a3^2) *(0.81 - 1.13*w1/w3)*d3^4/d1^4 +1.00 + 1.12*d3/d1 - 1.08*d3^3/d1^3 + k93 r d3
  where k93 r d3 = 0.57 - 1.07*sqrt (r/d3) - 2.13*(r/d3) + 8.24*(r/d3)**1.5 - 8.48*(r/d3)^2 +2.9*(r/d3)**2.5
        a3 = area d3

divergingFlowFromBranch :: (Ord a ,Floating a) => a -> a -> a -> a -> a -> a -> a -> a -> a -> Either String a
divergingFlowFromBranch w3 w2 w1 d3 d2 d1 g rho r
  | d1 /= d2  = Left "No formula for divergingFlowFromBranch with non unity branches"
  | w2/w1 <= 0.2 && w2/w1 >= 0.8= Left  "No formula for divergingFlowFromBranch outsie w2/w1 >=0.2 && w2/w1 <= 0.8"
  | otherwise  =  Right $ w2^2/(2*g*rho*a2)* (1.59*w1^2/w2^2 + (1.18 - 1.84*sqrt(r/d) + 1.16*r/d)*w1/w2 - 1.68 + 1.04*sqrt(r/d) - 1.16*r/d)
    where a2 = area d2
          d = d1

cm :: Floating a => a -> a -> a
cm r d3 = 0.23 + 1.46*(r/d3) - 2.75*(r/d3)**2 + 1.83*(r/d3)**3
cxc :: Floating a => a -> a -> a
cxc r d3 = 0.08 + 0.56*(r/d3) - 1.75*(r/d3)**2 + 1.83*(r/d3)**3
cyc :: Floating a => a -> a -> a -> a
cyc r d1 d3 = 1 - 0.25*(d3/d1)**1.3 - (0.11 * r /d3 - 0.65*r**2/d3**2 + 0.83*r**3/d3**3)*d3**2/d1**2

convergentFlowThroughRun :: Floating a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
convergentFlowThroughRun  w3 w2 w1 d3 d2 d1 g rho r = w2**2/(2*g*rho*a1**2)*(2*w1**2/w2**2 - 1.95 - 2*cxc r d3 *(w1/w2 -1) - 2*cm r d3 *(w1**2/w2**2 - w1/w2))
  where
        a1 = area d1

convergentFlowThroughBranch :: Floating a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
convergentFlowThroughBranch w3 w2 w1 d3 d2 d1 g rho r = w3**2/(2*g*rho*a3**2)*((2*cyc r d1 d3 -1) + d3**4/d1**4*(2*(cxc r d3 - 1) + 2*(2 - cxc r d3 - cm  r d3 )*w1/w3 + 0.08*w1**2/w3**2))
  where a3 = area d3

convergentFlowIntoBranch :: (Eq a,Floating a ) => a -> a -> a -> a -> a -> a -> a -> a -> a -> Either String a
convergentFlowIntoBranch  w3 w2 w1 d3 d2 d1 g rho r
  | d1 /= d2  = Left "No formula for convergentFlowIntoBranch with non unity branches"
  | otherwise  =  Right $ w2**2/(2*g*rho*a2)* ((1.18 - 1.16*sqrt(r/d) + 0.5*r/d)*w1**2/w2 - (0.95 - 1.65*r/d)*w1/w2 + 0.34 -1.69*r/d)
    where a2 = area d2
          d = d1

area :: Floating a =>  a -> a
area d = pi*(d/2)**2

