
{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Debug.Trace
import Plane
import Data.Monoid
import Control.Applicative
import Linear.V3
import Equation
import Domains
import Grid
import Project
import Force
import Input
import qualified Data.Foldable as F

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)

main = mapM solve $ zip [0..] [exampleSurf] -- [example1,example2,example3]
solve (i,ex) = do

  let ini = makeIter 0 1 ex
      preres = printResidual ini momentForceEquations
  let iter = solveIter ( makeIter 0 1 ex) momentForceEquations
      posres = printResidual iter momentForceEquations

--   displayModel ("force-model-bend" <> show i ,grid $ ini )
  putStrLn $ "Jacobian: " <> show (printJacobian (realToFrac <$> ini) momentForceEquations)
  putStrLn $ "Pre Resídual: " <>  show preres
  putStrLn $ "Pos Resídual: " <>  show posres
  putStrLn $ "Resídual Improvement: " <>  show (zipWith (/) posres  preres)
  putStrLn $ "Node Forces:" <>  show (printResidual iter forces)
  putStrLn $ "Link Forces:" <>  show (printResidual iter forces)
  displayBended ("force-model-bend" <> show i ,iter )
  print (pressures iter)

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
  x1 <- node (Support (Tag (V3 0 0 0) 0 (V3 Nothing Nothing 0 ) 0 ) )
  x2 <- node (Support (Tag (V3  Nothing 0 0 ) 0 (V3 0 Nothing 0) 0 ))
  x3 <- node (Support (Tag (V3 Nothing Nothing 0) 0 (V3 2 1 0) 0 ))
  link [aco 10 1,BTurn (0,1/2)] x1 x2
  link [aturn 10 (-10),aco (sqrt (10^2 + 10^2)) (2*sqrt 2),aturn (-10) (-10)] x2 x3
  link [BTurn (0,1/4),aco 10 0.5,BTurn (0,-1/4)] x3 x1


example6 = fst $ runInput $ mdo
  x1 <- node (Support (Tag (V3 Nothing (-0.4) 0) 0 (V3 0 Nothing 0 ) 0 ) )
  x2 <- node (Support (Tag (V3 0 0.5 0 ) 0 (V3 Nothing Nothing 0) 0 ))
  x3 <- node (Support (Tag (V3 Nothing Nothing 0) 0 (V3 2 1 0) 0 ))
  link [aco 10 1,BTurn (0,1/2)] x1 x2
  link [aturn  10 (-10) , aco (sqrt (10^2 + 10^2)) (2*sqrt 2),aturn (-10) (-10)] x2 x3
  link [BTurn (0,1/4),aco 10 0.5,BTurn (0,-1/4)] x3 x1


fixed = node (Support (Tag (V3 0 0 0 ) 0 (V3  Nothing Nothing 0 )0 ))
pin = node (Support (Tag (V3 Nothing Nothing 0) 0 0 0 ))
roller = node (Support (Tag (V3  Nothing 0 0) 0 (V3 0 Nothing 0) 0 ))
aload load = node (Support (Tag (V3 Nothing Nothing 0) 0 (V3 0 (-load ) 0) 0 ))
aturn x y =BTurn (0, atan2 x y / (2*pi))
aturn2 x y z =BTurn (-atan2 z x /(2*pi), atan2 y x / (2*pi))

turn l x y = (l , atan2 x y /(2*pi))

example9 = fst $ runInput $ mdo
  x1 <- conn [turn l1 4 3] (Tag (V3 Nothing Nothing 0 ) 0 0 0)
  x0 <- conn [turn l1 (-4) 3] (Tag (V3 Nothing Nothing 0 ) 0 0 0)
  let b = Bar 50 1000 5
  l1 <- link [b] x1 x0
  return ()

conn l c = node (Connection  ((\((i,_),a) -> (i,a)) <$> l ) c)

example10 = fst $ runInput $ mdo
  x0 <- node (Support (Tag (V3 0 0 0) (V3 0 0 0 ) (V3 Nothing Nothing Nothing ) (V3  Nothing Nothing Nothing ) ))
  l1 <- link ([ BTurn (-0.1,0.1) ] <> replicate 5 (Beam 7 100 5 250 250)) x0 x1
  x1 <- node (Support (Tag (V3 Nothing  Nothing Nothing ) (V3 0 0 0 ) (V3 (-10) (-10) (-10)) 0 ))
  return ()


example8 = fst $ runInput $ mdo
  x0 <- node (Support (Tag (V3 Nothing Nothing  0) (V3 0 0 Nothing ) (V3 0 0 0 ) (V3  0 0 0 ) ))
  link [aturn 4 3, Beam 5 100 125 250 250,aturn 4 (-3)] x0 x1
  x1 <- node (Support (Tag (V3 Nothing  Nothing 0 ) (V3 0 0 Nothing ) (V3 0 (0) 0) 0))
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

exampleSurf = fst $ runInput $ mdo
  let em = 100
      a =  5
      ba = 10
  x1 <- conn [turn l1 0 1 ,turn l4 1 0] (Tag 0 0  (V3 Nothing Nothing 0 ) 0)
  l1 <- link [Link (2*a )  ] x1 x2
  x2 <- conn [turn l1 0 1 ,turn l2 1 0] (Tag (V3 Nothing 0 0) 0 (V3 0 Nothing 0 ) 0)
  l2 <- link [Link (a) ] x2 x3
  x3 <- conn [turn l2 0 1, turn l3 1 0]  (Tag (V3 Nothing Nothing  0) 0 (V3 10 10 0 ) 0)
  l3 <- link [Link (2*a) ] x3 x4
  x4 <- conn [turn l3 0 1,turn l4 1 0]  (Tag (V3 0 Nothing 0)  0 (V3 Nothing 0 0 ) 0 )
  l4 <- link [Link (a)  ] x4 x1
  surface (Quad4 (ematQ 96 (1/3)) 1) [l1,l2,l3,l4]
  return ()


