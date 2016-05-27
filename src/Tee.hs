{-# LANGUAGE TupleSections,NoMonomorphismRestriction #-}
module Tee where

import Debug.Trace
import Linear.Metric
import Data.Monoid
import Domains
import Control.Applicative
import qualified Data.Map as M
import Linear.V2
import Linear.Matrix
import Hydraulic
import TBL.Parser
import Data.Maybe
import GHC.Stack
--
-- From Pipe Flow A Pratical and Comprehensive Guide - Chapter Tee - page 170 to 199
--
eps = 1e-12

-- g =  32.174
g =  9.81

trilinear  i j = fromMaybe 0 $ trilinearInterp i (buildTable3D j)

allMaybes i
  | all isJust i = fmap fromJust i
  | otherwise = []

fromJustE e (Just i) = i
fromJustE e i = errorWithStackTrace $ "fromJustE" <> e

biinterp  j m = fromMaybe 0 $ bilinearInterp  j (buildTable2D m)
liinterp  j m = fromMaybe 0 $ linearInterp j (buildTable1D m)


lossPa l fluid flowMap section = fmap (pressureDrop fluid flowMap section. regularizeLoss l flowMap)

pressureDrop fluid flowMap section = (\(i,e) -> (i, density fluid *e  * (abs (var i flowMap )/areaS (var i section))^2/2))
regularizeLoss l  flowMap = (\(i,e)-> (i,e/ ((var i flowMap)^2/(var l flowMap)^2) ))

sectionMap t = M.fromList $ zip (teeConfig t) (teeSection t)

fittingLosses
  :: (Floating a, Ord a, Show a) =>
     Fluido a
     -> M.Map String [(String,TableType a )] -> M.Map Int a -> TeeConfig a -> [(Int, a)]
fittingLosses fluid tblmap flowMap t@(TeeConfig [ix] [sec] int)
    =  case int of
    RectangularEntry theta l wall ->
        case wall of
          False -> lossess "Er1-2" recentry
          True -> lossess "Er1-3" recentry
      where
        recentry  = [(ix,biinterp (theta,l/hydraulicDiameter sec))]
    DuctEntry len th -> case sec  of
       Rectangular _ _  -> lossess "Er1-1" ductentry
       Circular _ -> lossess "Ed1-1" ductentry
      where
        ductentry = [(ix,biinterp (len/hydraulicDiameter sec,th/hydraulicDiameter sec) )]

    RoundEntry r wall ->
      traceShowId $ case wall of
        True -> lossess "Ed1-2" roundentry
        False -> lossess "Ed1-3" roundentry
      where
        roundentry =  [(ix,liinterp (r/hydraulicDiameter sec))]
  where
    lossess elt fun = zipWith (\f e -> fmap ($f) e) (fmap snd $ var elt  tblmap) fun
fittingLosses fluid tblmap flowMap t@(TeeConfig [ix,ox] [isec,osec] el )
  =  case el of
    Screen n a1  -> lossess "Cd9-1" screen
      where
        screen = [(ox, biinterp (n, a1/areaS osec) )]
    Transition theta -> case (osec,isec) of
      (Circular _ ,Rectangular _ _ )->  lossess "Er4-3" transition
      (Rectangular _ _ ,Circular  _ )->  lossess "Er4-3" transition
      i -> errorWithStackTrace $ "no transition " <> show i
      where
        transition = [(ox, biinterp (theta,areaS osec/areaS isec) )]
    FireDamper -> lossess "Cd9-3" [(ox, constantInterp .buildTable0D)]
    Damper theta d  ->  case osec of
          Circular _ ->  lossess "Cd9-1" damper
          Rectangular _ _ ->  lossess "Cr9-4" damper
      where
        damper = [(ox,biinterp (theta,d/hydraulicDiameter osec))]
    Elbow ang radius constr ->
      case (ang,constr) of
        (45,Pleated) -> lossess "Cd3-7" elbow
        (45,Mitted) -> lossess "Cd3-17" elbow
        (90,Gored 5) -> lossess "Cd3-9" elbow
        (90,Gored 3) -> lossess "Cd3-12" elbow
        (60,Gored 3) -> lossess "Cd3-13" elbow
        (45,Gored 3) -> lossess "Cd3-14" elbow
        i -> errorWithStackTrace $ "elbow not implemented" <> show i
      where elbow = [(ox,liinterp (hydraulicDiameter osec *1000) )]
    FanSystemInteraction (Elbow ang radius constr) len fan ->
      case (fan,ang ,constr) of
        (Centrifugal,90,Gored 4 ) -> lossess "Ed7-2" fanE
      where fanE = [(ox,biinterp (len/hydraulicDiameter osec ,radius/hydraulicDiameter osec ) )]
  where
    lossess elt fun = zipWith (\f e -> fmap ($f) e) (fmap snd $ var elt  tblmap) fun

fittingLosses fluid tblmap flowMap t@(TeeConfig [rli,bi,rri] [amain , abranch ,aramal]  int )
  = regularizeLoss rli flowMap <$> case int  of
     RectangularTee _ ->  classifyFlow
      where
        classifyFlow
          |  rls > 0 && bs <= 0 && rrs <= 0 = lossess "Sr5-5" rectee
          |  otherwise  = []
        rectee = zip [rri,bi]  [liinterp (rrs/rls) , biinterp (bs/rls,areaS abranch /areaS amain)]
     RoundTee _ _ _ ->  classifyFlow
      where
        classifyFlow
          |  rls < 0 && bs <= 0 && rrs >= 0 = lossess "Ed5-1" roundtee
          | otherwise = []
        roundtee = zip [rri,bi] [trilinear (bs/rls,areaS aramal /areaS amain  ,areaS abranch /areaS amain) , trilinear (abs $ rrs/rls,areaS abranch /areaS amain,areaS aramal /areaS amain  ) ]
  where
    [rls,bs,rrs]  = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in m@ap " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
    lossess elt fun = zipWith (\f e -> fmap ($f) e) (fmap snd $ var elt  tblmap) fun


fittingLossesNFPA _ joelhos flowMap  t =  classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        classifyFlow bl =   classifyFlow' t flowMap bl




classifyFlow' t flowMap bl@[rls,bs,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 = zip [rri,bi]  [direct,lateral]
          |  rrs > 0 && bs <= 0 && rls <= 0 = zip [rli,bi]  [direct ,lateral]
          |  rls >= 0 && bs >= 0 && rrs < 0 = zip [rli,bi]  [direct ,lateral]
          |  rrs >= 0 && bs >= 0 && rls < 0 = zip [rri,bi]  [direct ,lateral]
          |  bs > 0 && rrs <= 0 && rls <= 0 = zip [rli,rri] [lateral ,lateral]
          |  bs < 0 && rrs >= 0 && rls >= 0 = zip [rli,rri] [lateral ,lateral]
          | otherwise =  traceShow ("no case for branch list " ++ show  t ++ show flowMap) []
        where
              [rli,bi,rri] = teeConfig t
              [dr,db,_] = teeSection t
              rho = teeMaterial (teeInternal t)
              direct = Perda $TabelaPerda (dr) ("Conexao","Te","Direta")  rho
              lateral = Perda $TabelaPerda (db) ("Conexao","Te","Lateral")  rho
classifyFlow' t flowMap bl@[rls,bs,bo,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 = zip [rri,bi,bu]  [direct,lateral,lateral]
          |  rrs > 0 && bs <= 0 && rls <= 0 = zip [rli,bi,bu]  [direct ,lateral,lateral]
          |  rls >= 0 && bs >= 0 && rrs < 0 = zip [rli,bi,bu]  [direct ,lateral,lateral]
          |  rrs >= 0 && bs >= 0 && rls < 0 = zip [rri,bi,bu]  [direct ,lateral,lateral]
          |  bs > 0 && rrs <= 0 && rls <= 0 = zip [rli,rri,bu] [lateral ,lateral,direct]
          |  bs < 0 && rrs >= 0 && rls >= 0 = zip [rli,rri,bu] [lateral ,lateral,direct]
          |  bo > 0 && rrs <= 0 && rls <= 0 = zip [rli,rri,bi] [lateral ,lateral,direct]
          |  bo < 0 && rrs >= 0 && rls >= 0 = zip [rli,rri,bi] [lateral ,lateral,direct]
          | otherwise =  traceShow ("no case for branch list " ++ show  t ++ show flowMap) []
          where
              [rli,bi,bu,rri] = teeConfig t
              [dr,db,_] = teeSection t
              rho = teeMaterial (teeInternal t)
              direct = Perda $TabelaPerda (dr) ("Conexao","Te","Direta")  rho
              lateral = Perda $TabelaPerda (db) ("Conexao","Te","Lateral")  rho

{-
fittingLosses fluid Formula flowMap  t@(TeeConfig _ [Circular dr,Circular db,_]  (RoundTee _  _ _)) =  fmap (/1000) <$> classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        [rli,bi,rri] = teeConfig t
        r =  teeRadius (teeInternal t)
        rho = teeMaterial (teeInternal t)
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

-}
