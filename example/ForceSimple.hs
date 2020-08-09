{-# LANGUAGE RankNTypes,TypeApplications,ScopedTypeVariables,FlexibleInstances,RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Debug.Trace
import qualified Data.Text as T
import Plane
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Linear.V3
import Equation
import Backend.Graphviz
import Domains
import Grid
import Numeric
import Project
import Position
import Force
import Input
import qualified Data.Foldable as F
import Backend.Mecha (openSCAD)

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)


main = do 
  solve example8

solve ::  (forall a . (RealFloat a, Show a ) => Grid Force a) -> IO ()
solve gmodel  = do
  solveSystem gmodel momentForceEquations
  return ()

  -- putStrLn $ "Jacobian: " <> show (printJacobian ( initIter 2 ) (momentForceEquations ) )
  -- putStrLn $ "Pre Resídual: " <>  show preres
  -- putStrLn $ "Pos Resídual: " <>  show posres
  -- putStrLn $ "Resídual Improvement: " <>  show (zipWith (/) posres  preres)
  -- putStrLn $ "Node Forces:" <>  show (printResidual iter forces)
  -- putStrLn $ "Link Forces:" <>  show (printResidual iter forces)
  -- displayBended ("force-model-bend" <> show i ,iter )
  -- displaySolve ("force-model-bend" <> show i ,iter )
  -- print (pressures iter)


{-
example1 =  fst $ runInput $ mdo
   r1 <- node (Support Pin)
   link [Bar 2,Load2D 400 300 , Bar 2,BarTurn (60/360),Bar 2 ] r1 r2
   r2 <- node (Support Roller)
   return ()

example2 =  fst $ runInput $ mdo
   r1 <- node (Support FixedSupport2D)
   link [Bar 2,Load2D 0 1600,Bar 2] r1 r2
   r2 <- node (Support Pin)
   link [Bar 1,Load2D 0 600,Bar 2] r2 r3
   r3 <- node (Support Roller)
   return ()

example3 = fst $ runInput $ mdo
  r1 <- node (Support (Friction 0.3))
  link [BarTurn (1/4) ,Bar 2,BarTurn (-1/4),Bar 3, Load2D 0 500,Bar 3 , BarTurn (1/4) , BarTurn (-1/6) ,Load,BarTurn (-1/12),BarTurn (-1/4) ,Bar 2,BarTurn (1/4) ] r1 r2
  r2 <- node (Support (Friction 0.3))
  return ()


example4  = fst $ runInput $ mdo
  x1 <- node (Support FixedSupport3D)
  x2 <- node (Support FixedSupport3D)
  x3 <- node (Support FixedSupport3D )
  x4 <- node (Support FixedSupport3D)
  let h = 2.5
      d = 4.0
      col =  link [BTurn (0.25,0), Bar h ,BTurn (-0.25,0) ]
      vg = [BTurn (0,-0.25),Bar (d/2),Load3D (V3 0 0(-500)) 0,Bar (d/2)]
  link [BTurn (-0.35,0.25), Bar 2 ,BTurn (0.35,0), BTurn (0.5,0),Load3D (V3 0 0 500) 0  ,BTurn (-0.5,0)] x1 x4
  link [BTurn (0.45,0.25), Bar 3 ,BTurn (-0.45,0) ] x4 x2
  link [BTurn (0.15,-0.25), Bar 4 ,BTurn (-0.25,0) ] x4 x3
-}





example5 = fst $ runInput $ mdo
  x1 <- conn [turn l1 1 0,turn l3 0 1] (Forces (V3 0 0 0 ) 0 (V3  Nothing Nothing 0 )0)
  x2 <- conn [turn l2 1 1,turn l1 1 0] (Forces (V3  Nothing 0 0 ) 0 (V3 0 Nothing 0) 0)
  x3 <- conn [turn l2 1 1,turn l3 0 1] (Forces (V3 Nothing Nothing 0) 0 (V3 2 1 0) 0 )
  l1 <- link [aco 10 1] x1 x2
  l2 <- link [aco (sqrt (10^2 + 10^2)) (2*sqrt 2)] x2 x3
  l3 <- link [aco 10 0.5] x3 x1
  return ()


example6 = fst $ runInput $ mdo
  x1 <- support (Forces (V3 Nothing (-0.4) 0) 0 (V3 0 Nothing 0 ) 0 )
  x2 <- support (Forces (V3 0 0.5 0 ) 0 (V3 Nothing Nothing 0) 0 )
  x3 <- support (Forces (V3 Nothing Nothing 0) 0 (V3 2 1 0) 0 )
  link [aco 10 1,BTurn (0,1/2)] x1 x2
  link [aturn  10 (-10) , aco (sqrt (10^2 + 10^2)) (2*sqrt 2),aturn (-10) (-10)] x2 x3
  link [BTurn (0,1/2),aco 10 0.5,BTurn (0,-1/2)] x3 x1


support f = node (Support f)
fixed = support (Forces (V3 0 0 0 ) 0 (V3  Nothing Nothing 0 )0 )
pin = support (Forces (V3 Nothing Nothing 0) 0 0 0 )
roller = support (Forces (V3  Nothing 0 0) 0 (V3 0 Nothing 0) 0 )
aload load = support (Forces (V3 Nothing Nothing 0) 0 (V3 0 (-load ) 0) 0 )
aturn x y = BTurn (0, atan2 x y / (2*pi))

turn l x y = (l , (0,0,atan2 x y /(2*pi)))
rise l x y =(l, (0,atan2 x y / (2*pi),0))

example9 = fst $ runInput $ mdo
  x1 <- conn [turn l1 4 3] (Forces (V3 Nothing Nothing 0 ) 0 0 0)
  x0 <- conn [turn l1 (-4) 3] (Forces (V3 Nothing Nothing 0 ) 0 0 0)
  let b = Bar 50 1000 5
  l1 <- link [b] x1 x0
  return ()

conn l c = node (Connection  ((\((i,_),a) -> (i,a)) <$> l ) c)

example10 = fst $ runInput $ mdo
  x0 <- support (Forces (V3 0 0 0) (V3 0 0 0 ) (V3 Nothing Nothing Nothing ) (V3  Nothing Nothing Nothing ))
  l1 <- link ([ BTurn (-0.1,0.1) ] <> replicate 5 (Beam 7 100 5 250 250)) x0 x1
  x1 <- support (Forces (V3 Nothing  Nothing Nothing ) (V3 0 0 0 ) (V3 (-10) (-10) (-10)) 0 )
  return ()


example8 = fst $ runInput $ mdo
  x0 <- support (Forces (V3 Nothing Nothing  0) (V3 0 0 Nothing ) (V3 0 0 0 ) (V3  0 0 0 ) )
  link [aturn 4 3, Beam 5 100 125 250 250,aturn 4 (-3)] x0 x1
  x1 <- support (Forces (V3 Nothing  Nothing 0 ) (V3 0 0 Nothing ) (V3 0 (0) 0) 0)
  return ()

example7 = fst $ runInput $ mdo
  x0 <- fixed
  let
    atop = 10
    abot = 2
    abat = 3
    adia = 1
    em = 1000
    aco l s = Bar l em s
  link [aco 10 abot, BTurn (0,1/2)] x0 x2
  link [aturn (-5) (-10), aco (sqrt 125) atop,aturn (-5) 10] x1 x0
  x1 <- pin
  link [BTurn (0,1/4) ,aco 5 abat ,BTurn (0,1/4)] x2 x1
  x2 <- aload 10
  link [aturn 3 10,aco  (sqrt 109) atop , aturn (-3) (10),BTurn (0,1/2)] x1 x3
  x3 <- pin
  link [aco 10 abot , BTurn (0,1/2)] x2 x4
  x4 <- aload 10
  link [BTurn (0,-1/4) , aco 8 abat ,BTurn (0,-1/4)] x3 x4
  link [aturn (-5) (10),aco (sqrt 125) adia  ,aturn (5) (-10)] x1 x4
  link [aturn 1 10,aco  (sqrt 101) atop , aturn (-1) (10),BTurn (0,1/2)] x3 x5
  x5  <- pin
  link [BTurn (0,-1/4) , aco 9 abat ,BTurn (0,1/4)] x5 x6
  link [aturn (8) (-10), aco (sqrt 164) adia ,aturn (8) (10)] x6 x3
  link [aco 10 abot ,BTurn (0,1/2)] x4 x6
  x6 <- aload 16
  link [aturn 1 (-10),aco  (sqrt 101) atop , aturn (-1) (-10),BTurn (0,1/2)] x7 x5
  x7 <- pin
  link [BTurn (0,1/4) , aco 8 abat ,BTurn (0,1/4)] x8 x7
  link [aco 10 abot , BTurn (0,1/2)] x6 x8
  link [aturn 8 10, aco (sqrt 164) adia ,aturn (-8) (10)] x6 x7
  x8 <- aload 10
  link [aco 10 abot , BTurn (0,1/2)] x8 x10
  link [aturn (-3) 10,aco  (sqrt 109) atop , aturn (-3) (-10)] x7 x9
  x9 <- pin
  link [aturn (-5) (-10), aco (sqrt 125) adia ,aturn (5) (-10)] x9 x8
  link [BTurn (0,-1/4) , aco 5 abat  ,BTurn (0,-1/4)] x9 x10
  x10 <- aload 10
  link [aturn (-5) 10, aco (sqrt 125) atop,aturn (5) 10,BTurn (0,1/2)] x9 x11
  link [aco 10 abot , BTurn (0,1/2)] x10 x11
  x11 <- roller
  return ()

aco l s = Bar l 100 s

cw = first (True,)
ccw  = first (False ,)

{-
ClearAll[Em,\[Nu],th];
Em=10000; \[Nu]=.25;  th=3; aspect=6/5; Nsub=4;

Emat=Em/(1-\[Nu]^2)*{{1,\[Nu],0},{\[Nu],1,0},{0,0,(1-\[Nu])/2}};

(*  Define FEM model *)

NodeCoordinates=N[{{0,6},{0,0},{5,6},{5,0}}];
PrintPlaneStressNodeCoordinates[NodeCoordinates,"",{6,4}];
ElemNodes=  {{1,2,4,3}};
numnod=Length[NodeCoordinates]; numele=Length[ElemNodes];
ElemTypes=       Table["Quad4",{numele}];
PrintPlaneStressElementTypeNodes[ElemTypes,ElemNodes,"",{}];
ElemMaterials=   Table[Emat,   {numele}];
ElemFabrications=Table[th,     {numele}];
PrintPlaneStressElementMatFab[ElemMaterials,ElemFabrications,"",{}];
NodeDOFValues=NodeDOFTags=Table[{0,0},{numnod}];
NodeDOFValues[[1]]=NodeDOFValues[[3]]={0,75}; (* nodal loads *)
NodeDOFTags[[1]]={1,0};     (* vroller @ node 1 *)
NodeDOFTags[[2]]={1,1};     (* fixed node 2     *)
NodeDOFTags[[4]]={0,1};     (* hroller @ node 4 *)
PrintPlaneStressFreedomActivity[NodeDOFTags,NodeDOFValues,"",{}];
ProcessOptions={True};
Plot2DElementsAndNodes[NodeCoordinates,ElemNodes,aspect,
    "One element mesh - 4-node quad",True,True];

(*  Solve problem and print results *)

{NodeDisplacements,NodeForces,NodePlateCounts,NodePlateStresses,
    ElemBarNumbers,ElemBarForces}= PlaneStressSolution[
    NodeCoordinates,ElemTypes,ElemNodes,
    ElemMaterials,ElemFabrications,
    NodeDOFTags,NodeDOFValues,ProcessOptions];

PrintPlaneStressSolution[NodeDisplacements,NodeForces,NodePlateCounts,
      NodePlateStresses,"Computed Solution:",{}];

(*  Plot Averaged Nodal Stresses Distribution *)

sigeps=10.^(-3); nbands=10;
sxx=Table[NodePlateStresses[[n,1]],{n,numnod}];
syy=Table[NodePlateStresses[[n,2]],{n,numnod}];
sxy=Table[NodePlateStresses[[n,3]],{n,numnod}];
{sxxmax,syymax,sxymax}=Abs[{Max[sxx],Max[syy],Max[sxy]}]+sigeps;
{sxxmin,syymin,sxymin}=Abs[{Min[sxx],Min[syy],Min[sxy]}]+sigeps;
sxxmax=Max[sxxmax,sxxmin];  sxxmin=-sxxmax;
syymax=Max[syymax,syymin];  syymin=-syymax;
sxymax=Max[sxymax,sxymin];  sxymin=-sxymax;
{sxxinc,syyinc,sxyinc}={sxxmax-sxxmin,syymax-syymin,sxymax-sxymin}/nbands;
Print["sxxmin,sxxmax,sxxinc=",{sxxmin,sxxmax,sxxinc}];
ContourBandPlotNodeFuncOver2DMesh[NodeCoordinates,ElemNodes,sxx,{sxxmin,sxxmax,sxxinc},
  {True,True,True,False,True,True},{2,2},aspect,"Stress sigma-xx"];
Print["syymin,syymax,syyinc=",{syymin,syymax,syyinc}];
ContourBandPlotNodeFuncOver2DMesh[NodeCoordinates,ElemNodes,syy,{syymin,syymax,syyinc},
  {True,True,True,False,True,True},{2,2},aspect,"Stress sigma-yy"];
Print["sxymin,sxymax,sxyinc=",{sxymin,sxymax,sxyinc}];
ContourBandPlotNodeFuncOver2DMesh[NodeCoordinates,ElemNodes,sxy,{sxymin,sxymax,sxyinc},
  {True,True,True,False,True,True},{2,2},aspect,"Stress sigma-xy"];

-}
quad9ex ::  RealFloat a  => Grid Force a  
quad9ex = fst $ runInput $ mdo
  let em = 10000
      a = 5
      ba = 6
      nu = 1/4
      th = 3
      nsub = 4
  x1 <- conn [turn l1 0 1] (Forces 0 0  (V3 Nothing Nothing 0 ) 0)
  l1 <- link [Link a] x1 x2
  x2 <- conn [turn l1 0 1 ,turn l2 1 0] (Forces (V3 Nothing 0 0) 0 (V3 0 Nothing 0 ) 0)
  l2 <- link [Link ba] x2 x3
  x3 <- conn [turn l2  (-1) 0 , turn l3 0 1] (Forces (V3 Nothing Nothing  0) 0 (V3 20 75 0) 0)
  l3 <- link [Link a] x3 x4
  x4 <- conn [turn l3 0 (-1),turn l4 (-1) 0] (Forces (V3 0 Nothing 0)  0 (V3 Nothing 75 0 ) 0 )
  l4 <- link [Link ba] x4 x1
  surface (Quad4 (ematQ em nu) th ) [cw l1,cw l2,cw l3,cw l4]
  return ()



exampleSurf = fst $ runInput $ mdo
  let em = 1000
      a = 5
      ba = 6
      nu = 1/4
      th = 3
  x1 <- conn [turn l1 0 1] (Forces 0 0  (V3 Nothing Nothing 0 ) 0)
  l1 <- link [Link a] x1 x2
  x2 <- conn [turn l1 0 1 ,turn l2 1 0] (Forces (V3 Nothing 0 0) 0 (V3 0 Nothing 0 ) 0)
  l2 <- link [Link ba] x2 x3
  x3 <- conn [turn l2  (-1) 0 , turn l3 0 1] (Forces (V3 Nothing Nothing  0) 0 (V3 0 75 0) 0)
  l3 <- link [Link a] x3 x4
  x4 <- conn [turn l3 0 (-1),turn l4 (-1) 0] (Forces (V3 0 Nothing 0)  0 (V3 Nothing 75 0 ) 0 )
  l4 <- link [Link ba] x4 x1
  surface (Quad4 (ematQ em nu) th ) [cw l1,cw l2,cw l3,cw l4]
  return ()


incr a  ~((s1,(l1,l2,l3,l4),(x1,x2,x3,x4)),(l13,l14,l15,l16)) = mdo
  let
    em = 480
    v=1/3
    sf = FaceLoop
    free= Forces (V3 Nothing Nothing Nothing ) 0  (V3  0 0 0 ) 0
  l5 <- link [Link a] x1 x5
  l6 <- link [Link a] x2 x6
  l7 <- link [Link a] x3 x7
  l8 <- link [Link a] x4 x8
  x5 <- conn [turn l9 0 (-1),rise l5 1 0  ,turn l12 (-1) 0 ,rise l13  (-1) 0]  free
  l9 <- link [Link a] x5 x6
  x6 <- conn [turn l10 (1) 0 ,rise l6 1  0 , turn l9 0 (-1) ,rise l14  (-1) 0 ]  free
  l10 <- link [Link a] x6  x7
  x7 <- conn [turn l11   0 (1)   ,rise l7  1 0,turn l10  (1) 0 ,rise l15  (-1) 0    ]  free
  l11 <- link [Link a] x7  x8
  x8 <- conn [turn l12  1 0 ,rise l8 1 0,turn l11  0 (-1) ,rise l16  (-1) 0 ]  free
  l12 <- link [Link a] x8 x5
  s2 <- surface sf [ccw l9,ccw l10,ccw l11,ccw l12]
  s3 <- surface sf [cw l1,cw l6 , ccw l9,ccw l5]
  s4 <- surface sf [cw l2,cw l7 , ccw l10,ccw l6]
  s5 <- surface sf [cw l3,cw l8 , ccw l11,ccw l7]
  s6 <- surface sf [cw l4,cw l5 , ccw l12,ccw l8]
  polyhedra (Hexa8 (ematT em v)) $ ccw s1 : fmap cw [s2,s3,s4,s5,s6]
  return ((s2,(l9,l10,l11,l12),(x5,x6,x7,x8)),(l5,l6,l7,l8))

hexahedron8 = fst $ runInput $ mdo
  let
    a = 4
    l =  4
    em=480
    v=1/3
    sf = FaceLoop
    free2 = Forces (V3 Nothing Nothing Nothing ) 0  (V3 30 0 0 ) 0
    free= Forces (V3 Nothing Nothing Nothing ) 0  (V3 0 0 0 ) 0
  x1 <- conn [turn l1 0 1 ,{-turn l4 (-1) 0,-}rise b5  (-1) 0  ] (Forces 0 0  (V3 Nothing Nothing Nothing ) 0)
  l1 <- link [Link a ] x1 x2
  x2 <- conn [turn l1 0 (-1) ,turn l2 (1) 0,rise b6 (-1) 0 ] (Forces (V3 0 0 0 ) 0 (V3 Nothing Nothing Nothing ) 0)
  l2 <- link [Link a ] x2 x3
  x3 <- conn [turn l2 1 0 , turn l3 0 (1)  ,rise b7 (-1) 0 ]  (Forces (V3 0 0 0 )  (V3 0 0 0  )(V3 Nothing Nothing  Nothing ) 0)
  l3 <- link [Link a ] x3 x4
  x4 <- conn [turn l3 0 (-1) ,turn l4 (1) 0,rise b8 (-1) 0 ]  (Forces  (V3 0 0 0)  0 (V3  Nothing Nothing Nothing ) 0 )
  l4 <- link [Link a ] x4 x1
  s0 <- surface sf [ccw l1,ccw l2,ccw l3,ccw l4]
  ((s1,(f1,f2,f3,f4),(a1,a2,a3,a4)),(b5,b6,b7,b8)) <- (incr 4 ) ((s0,(l1,l2,l3,l4),(x1,x2,x3,x4)),(l5,l6,l7,l8))
  l5 <- link [Link l ] a1 x5
  l6 <- link [Link l ] a2 x6
  l7 <- link [Link l ] a3 x7
  l8 <- link [Link l ] a4 x8
  x5 <- conn [turn l9 0 (-1),rise l5 1 0  ,turn l12 (-1) 0 ]  free2
  l9 <- link [Link a ] x6 x5
  x6 <- conn [turn l10 (1) 0 ,rise l6 1  0 , turn l9 0 (-1)  ]  free
  l10 <- link [Link a ] x6  x7
  x7 <- conn [turn l11   0 (1)   ,rise l7  1 0,turn l10  (1) 0     ]  free
  l11 <- link [Link a ] x8  x7
  x8 <- conn [turn l12  1 0 ,rise l8 1 0,turn l11  0 (-1) ]  free2
  l12 <- link [Link a ] x8 x5
  s2 <- surface sf [cw l9,ccw l10,cw l11,ccw l12]
  s3 <- surface sf [cw f1,cw l6 , cw l9,ccw l5]
  s4 <- surface sf [cw f2,cw l7 , ccw l10,ccw l6]
  s5 <- surface sf [cw f3,cw l8 , cw l11,ccw l7]
  s6 <- surface sf [cw f4,cw l5 , ccw l12,ccw l8]
  polyhedra (Hexa8 (ematT em v)) $ ccw s1: fmap cw [s2,s3,s4,s5,s6]
  return ()

merge (i,(a,b,c)) (k,(l,m,n)) = (i,(a+l,m+b,c+n))

tetragon = fst $ runInput $ mdo
  let
    a = 4
    em=400
    v=1/3
    d = (sqrt $ 2*a*a)
    sf = FaceLoop
    free2 = Forces (V3 Nothing Nothing Nothing ) 0  (V3 10 100 10) 0
    free= Forces (V3 Nothing Nothing Nothing ) 0  (V3 0 0 0 ) 0
  x1 <- conn [turn l1 0 1 ,rise l4  (-1) 1 ] (Forces 0 0  (V3 Nothing Nothing Nothing ) 0)
  l1 <- link [Link a] x1 x2
  x2 <- conn [turn l1 0 1, turn l2 (-1) 0, rise l5 (-1) 0] (Forces (V3 0 0 0 ) 0 (V3 Nothing Nothing Nothing ) 0)
  l2 <- link [Link a] x2 x3
  x3 <- conn [turn l2 1 0, turn l3 1 1, (l6,(0,-1/4,-1/8))]  (Forces (V3 0 0 0 )  (V3 0 0 0  )(V3 Nothing Nothing  Nothing ) 0)
  l3 <- link [Link d] x3 x1
  l4 <- link [Link d] x1 x4
  l5 <- link [Link a] x2 x4
  l6 <- link [Link d] x3 x4
  x4 <- conn [(l6,(0,1/4,-1/8)), rise l4 1 (1), rise l5 1 0]  free2
  s1 <- surface sf [cw l1,cw l2,cw l3]
  s3 <- surface sf [cw l1,cw l5 , ccw l4]
  s4 <- surface sf [cw l2,cw l6 , ccw l5]
  s6 <- surface sf [ccw l6,cw l3 , cw l4]
  polyhedra (Tetra4 (ematT em v)) $ cw <$> [s1,s3,s4,s6]
  return ()

thexahedron = fst $ runInput $ mdo
  let
    a = 4
    em=400
    v=1/3
    d = (sqrt $ 2*a*a)
    sf = FaceLoop
    free2 = Forces (V3 Nothing Nothing Nothing ) 0  (V3 100 (100)  100) 0
  x1 <- conn [turn l1 0 1 ,rise c1 (-1) 0 ,rise l5  (-1) 1 ,(d5,(0,-3/8,1/4)) ] (Forces 0 0  (V3 Nothing Nothing Nothing ) 0)
  l1 <- link [Link a ] x1 x2
  x2 <- conn [turn l1  0 1  ,turn l2 (-1) 0,rise l6 (-1) 0  ] (Forces (V3 0 0 0 ) 0 (V3 Nothing Nothing Nothing ) 0)
  l2 <- link [Link a  ] x2 x3
  x3 <- conn [turn l2 1 0, turn l3 (1) (1) ,(d7,(-1/4,1/2,-1/8)),(l7,(0,-1/4,1/8)),turn f4 0 1,rise e1 (-1) 0]  (Forces (V3 0 0 0 )  (V3 0 0 0  )(V3 Nothing Nothing  Nothing ) 0)
  l3 <- link [Link d] x3 x1
  l5 <- link [Link d] x1 x6
  l6 <- link [Link a] x2 x6
  l7 <- link [Link d] x3 x6

  s1 <- surface sf [cw l1,cw l2,cw l3]
  s3 <- surface sf [cw l1,cw l6 , ccw l5]
  s4 <- surface sf [cw l2,cw l7 , ccw l6]
  s6 <- surface sf [ccw l7,cw l3 , cw l5]

  -- t2
  x4 <-  conn [turn f4  0 1  ,turn f5 (-1) 0,rise l8 (-1) 0  ]  (Forces (V3 0 0 0 )  (V3 0 0 0  )(V3 Nothing Nothing  Nothing ) 0)
  x6 <- conn [turn c4 (-1) 1 ,(l7,(0,1/4,-1/8))]  free2
  x7 <- conn [turn c4 1 1 ]  free2
  f4 <- link [Link a  ] x3 x4
  f5 <- link [Link a] x4 x1

  d7 <- link [Link d] x3 x7
  d5 <- link [Link d] x1 x7
  l8 <- link [Link a] x4 x7

  d1 <- surface sf [cw f4,cw f5,ccw l3 ]
  d2 <- surface sf [cw f5,cw d5,ccw l8 ]
  d3 <- surface sf [ccw f4,cw d7,ccw l8 ]
  d4 <- surface sf [cw l3 ,cw d5,ccw d7 ]


  -- t3
  x5 <-  conn [rise c1 (1) 0  ,turn c2 (-1) 0 , turn c3 0 (-1)] free2
  c1 <- link [Link a] x1 x5
  c2 <- link [Link a] x5 x7

  c3 <- link [Link a] x5 x6
  c4 <- link [Link d] x6 x7

  cs1 <- surface sf [cw c1 ,cw c2 ,ccw d5 ]
  cs2 <- surface sf [cw c3 ,cw c4 ,ccw c2 ]
  cs3 <- surface sf [cw c3 ,ccw l5,cw c1 ]
  cs4 <- surface sf [cw d5,ccw c4,ccw l5 ]


  -- t4
  x8 <-  conn [rise e1 (1) 0  ,turn e2  0 (1), turn e3  (1 ) 0]  free2
  e1 <- link [Link a] x3 x8
  e2 <- link [Link a] x8 x7

  e3 <- link [Link a] x8 x6

  es1 <- surface sf [cw e1 ,cw e2 ,ccw d7 ]
  es2 <- surface sf [cw e3 ,ccw l7 ,cw e1]
  es3 <- surface sf [cw e3 ,cw c4 ,ccw e2 ]
  es4 <- surface sf [cw c4 ,ccw d7,cw l7]


  polyhedra (Tetra4 (ematT em v)) $ cw <$> [cs2,cs3,cs4,cs1]

  polyhedra (Tetra4 (ematT em v)) $ [ccw s1,ccw s3,ccw s4,ccw s6]

  polyhedra (Tetra4 (ematT em v)) $ ccw <$> [d1,d2,d3,d4]
  polyhedra (Tetra4 (ematT em v)) $ ccw <$> [es1,es2,es3,es4]

  polyhedra (Tetra4 (ematT em v)) $ [ccw s6,cw d4,cw cs4,cw es4]
  return ()

