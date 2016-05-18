{-# LANGUAGE TupleSections,NoMonomorphismRestriction #-}
module Tee where

import Debug.Trace
import Linear.Metric
import Data.Monoid
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

ktubo t  v = perda*10*v**1.85
        where
              d  = diametroE t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))

biinterp  j m = fromMaybe 0 {-fromJustE ("no bileniar interp" <> show j)-} $ bilinearInterp  j (buildTable2D m)

classifyTee
  :: (Floating a, Ord a, Show a) =>
     Fluido a
     -> TeeConfigType a -> M.Map Int a -> TeeConfig a -> [(Int, a)]
classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig [_,bi] asection@[_,sec] (FanSystemInteraction (Elbow ang radius constr ) len fan ))
  = case (fan,ang ,constr) of
      (Centrifugal,90,Gored 4 ) -> lossess "Ed7-2"
      i -> errorWithStackTrace $ "elbow not implemented" <> show i
  where
    [_,vi]  = zipWith (\a q->abs q/areaS a) asection  bl
    bl = (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) <$> teeConfig t
    lossess t = zip [bi] $ zipWith (*) [density fluid * vi^2/2] [fromJustE ("no linear cs" <> show (hydraulicDiameter sec*1000,buildTable2D ta)) $  bilinearInterp (len/hydraulicDiameter sec ,radius/hydraulicDiameter sec ) (buildTable2D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table " <> t <>  show tblmap) $ M.lookup t tblmap

classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig [_,bi] asection@[_,sec] (Elbow ang radius constr ))
  = case (ang ,constr) of
      (45,Pleated) -> lossess "Cd3-7"
      (45,Mitted) -> lossess "Cd3-17"
      (90,Gored 5 ) -> lossess "Cd3-9"
      (90,Gored 3 ) -> lossess "Cd3-12"
      (60,Gored 3 ) -> lossess "Cd3-13"
      (45,Gored 3 ) -> lossess "Cd3-14"
      i -> errorWithStackTrace $ "elbow not implemented" <> show i
  where
    [_,vi]  = zipWith (\a q->abs q/areaS a) asection  bl
    bl = (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) <$> teeConfig t
    lossess t = zip [bi] $ zipWith (*) [density fluid * vi^2/2] [fromJustE ("no linear cs" <> show (hydraulicDiameter sec*1000,buildTable1D ta)) $  linearInterp (hydraulicDiameter sec *1000) (buildTable1D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table " <> t <>  show tblmap) $ M.lookup t tblmap
classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig [_,bi] asection@[_,sec] (Screen n a1 ))
  | otherwise = lossess "Cd9-1"
  where
    [_,vi]  = zipWith (\a q->abs q/areaS a) asection  bl
    bl = (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) <$> teeConfig t
    lossess t = zip [bi] $ zipWith (*) [density fluid * vi^2/2] [fromJustE "no linear cs" $  bilinearInterp (n, a1/areaS sec) (buildTable2D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table " <> t <>  show tblmap) $ M.lookup t tblmap
classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig [_,bi] asection@[ao,ai] (Transition theta) )
  | otherwise = case (ao,ai) of
                  (Circular _ ,Rectangular _ _ )->  lossess "Er4-3"
                  (Rectangular _ _ ,Circular  _ )->  lossess "Er4-3"
                  i -> errorWithStackTrace $ "no transition " <> show i
  where
    [_,vi]  = zipWith (\a q->abs q/areaS a) asection  bl
    bl = (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) <$> teeConfig t
    lossess t = zip [bi] $ zipWith (*) [density fluid * vi^2/2] [fromJustE "no linear cs" $  bilinearInterp (theta,areaS ao/areaS ai) (buildTable2D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table " <> t <>  show tblmap) $ M.lookup t tblmap
classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig [_,bi] asection@[_,sec] (FireDamper))
  | otherwise = lossess "Cd9-3"
  where
    [_,vi]  = zipWith (\a q->abs q/areaS a) asection  bl
    bl = (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) <$> teeConfig t
    lossess t = zip [bi] $ zipWith (*) [density fluid * vi^2/2] [constantInterp (buildTable0D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table " <> t <>  show tblmap) $ M.lookup t tblmap

classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig [_,bi] asection@[_,sec] (Damper theta d ))
  | otherwise = case sec of
                  Circular _ ->  lossess "Cd9-1"
                  Rectangular _ _ ->  lossess "Cr9-4"
  where
    [_,vi]  = zipWith (\a q->abs q/areaS a) asection  bl
    bl = (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) <$> teeConfig t
    lossess t = zip [bi] $ zipWith (*) [density fluid * vi^2/2] [fromJustE "no linear cs" $  bilinearInterp (theta,d/hydraulicDiameter sec) (buildTable2D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table " <> t <>  show tblmap) $ M.lookup t tblmap
classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig _ asection@[sec] (RectangularEntry theta l wall))
  | not wall = lossess "Er1-2"
  | otherwise = lossess "Er1-3"
  where
    [vbs]  = zipWith (\a q->abs q/areaS a) asection  bl
    [bi] = teeConfig t
    bl = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
    lossess t = zip [bi] $ zipWith (*) [density fluid * vbs^2/2] $[fromJustE "no linear cs" $  bilinearInterp (theta,l/hydraulicDiameter sec) (buildTable2D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table " <> t <>  show tblmap) $ M.lookup t tblmap
classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig _ asection@[d] (DuctEntry len th ))
  = case d of
    Rectangular _ _  ->  lossess "Er1-1"
    Circular _ ->  lossess "Ed1-1"
  where
    [vbs]  = zipWith (\a q -> abs q/areaS a) asection  bl
    [bi] = teeConfig t
    bl@[bs] = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
    lossess t = zip [bi] $ zipWith (*) [density fluid * vbs^2/2] $[fromJustE "no linear cs" $  bilinearInterp (len/hydraulicDiameter d,th/hydraulicDiameter d) (buildTable2D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table sr5-5" <> t <>  show tblmap) $ M.lookup t tblmap
classifyTee fluid (TBL tblmap) flowMap t@(TeeConfig _ asection@[d] (RoundEntry r wall))
  | not wall = lossess "Ed1-2"
  | otherwise = lossess "Ed1-3"
  where
    [vbs]  = zipWith (\a q -> abs q/areaS a) asection  bl
    [bi] = teeConfig t
    bl@[bs] = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
    lossess t = zip [bi] $ zipWith (*) [density fluid * vbs^2/2] $[fromJustE "no linear cs" $  linearInterp (r/hydraulicDiameter d) (buildTable1D ta)]
      where
        [ta] = fmap snd $ fromJustE ("no table sr5-5" <> t <>  show tblmap) $ M.lookup t tblmap

classifyTee fluid (TBL tblmap ) flowMap t@(TeeConfig _ asection@[amain , abranch ,aramal]  (RectangularTee _) ) = classifyFlow flow
  where
    [rli,bi,rri] = teeConfig t
    flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
    classifyFlow bl@[rls,bs,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 =
              let
                [t2d,ta] = fmap snd $ fromJustE "no table sr5-5" $ M.lookup "Sr5-5" tblmap
                -- cbm :: M.Map (a,a) a
                cbm = buildTable2D t2d
                bbm = buildTable1D ta
              in zip [rri,bi] $ zipWith (*) [density fluid * vbs^2/2, density fluid * vrrs^2/2] $zipWith (/) [fromJustE "no linear cs" $  linearInterp (rrs/rls) bbm , fromJustE "no bilinear cb" $ bilinearInterp (bs/rls,areaS abranch /areaS amain  ) cbm  ] [(vbs/vrls)^2,(vrrs/vrls)^2]

          | otherwise = []
      where [vrls,vbs,vrrs]  = zipWith (\a q -> abs q/areaS a) asection  bl
classifyTee fluid (TBL tblmap ) flowMap t@(TeeConfig  _ asection@[amain,abranch,aramal]  (RoundTee _  _ _)) = classifyFlow
  where
    [rli,bi,rri] = teeConfig t
    bl@[rls,bs,rrs]  = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in m@ap " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
    [vrls,vbs,vrrs]  = zipWith (\a q -> abs q/areaS a) asection  bl
    classifyFlow
          |  rls < 0 && bs <= 0 && rrs >= 0 =
              let
                [t2d,ta] = fmap snd $ fromJustE "no table sr5-5" $ M.lookup "Ed5-1" tblmap
              in zip [rri,bi] $ zipWith (*) [density fluid * vbs^2/2, density fluid * vrrs^2/2] $zipWith (/) [trilinear (bs/rls,areaS aramal /areaS amain  ,areaS abranch /areaS amain) ta , trilinear (abs $ rrs/rls,areaS abranch /areaS amain,areaS aramal /areaS amain  ) t2d ] [(vbs/vrls)^2,(vrrs/vrls)^2]
          | otherwise = [] -- errorWithStackTrace $ "flows" <> show (rls,bs,rrs)


classifyTee fluid Formula flowMap  t@(TeeConfig _ [Circular dr,Circular db,_]  (RoundTee _  _ _)) =  fmap (/1000) <$> classifyFlow flow
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

classifyTee _ Table   flowMap  t =  classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        classifyFlow bl = (\(ix,l) -> (ix,ktubo l (abs $ fromJustE "no ix" $ M.lookup ix flowMap)  )) <$>  classifyFlow' t flowMap bl

classifyTeeEl _  (TBL tblmap ) flowMap t@(TeeConfig _ _  (RectangularTee _ )) = classifyFlow flow
  where
    [rli,bi,rri] = teeConfig t
    flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
    [(_,Table2D n arr tmap ),(_,TableArray (_,ax) (_,ay))] = fromJustE "no table sr5-5" $ M.lookup "Sr5-5" tblmap
    -- cbm :: M.Map (a,a) a
    cbm = M.fromList $ concat $ fmap (\(ix,l) -> zipWith (\cb i-> ((i,ix),cb) ) l (snd arr)) (snd tmap)
    bbm = M.fromList $  zipWith (\i cb -> (i,cb) ) ax ay
    classifyFlow bl@[rls,bs,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 = zip [rri,bi]  [] -- [fromJustE "no bilinear cb" $ bilinearInterp (bs/rls,teeBranchArea t /teeMainArea t ) cbm  ,fromJustE "no linear cs" $  linearInterp (rrs/rls) bbm ]
          | otherwise = []
classifyTeeEl  _ Table flowMap  t =  classifyFlow flow
  where flow = fmap (\i -> fromJustE ("no variable " ++ show i ++ " in map " ++ show flowMap ) $ M.lookup  i flowMap) (teeConfig t)
        classifyFlow bl = classifyFlow' t flowMap bl
classifyTeeEl _ _ _ _ = []
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

