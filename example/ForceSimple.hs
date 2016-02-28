
{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Data.Monoid
import Linear.V3
import Equation
import Grid
import Project
import Force
import Input

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)

main = mapM solve $ zip [0..] [example1,example2,example3]
solve (i,ex) = do
  let iter = solveIter ( makeIter 0  1 ex) momentForceEquations
  displayModel ("force-model" <> show i ,ex )
  print $ printResidual iter momentForceEquations
  print iter

example1 =  fst $ runInput $ mdo
   r1 <- node (Support Pin)
   link [Beam 2,Load2D 400 300 , Beam 2,BeamTurn (60/360),Beam 2 ] r1 r2
   r2 <- node (Support Roller)
   return ()

example2 =  fst $ runInput $ mdo
   r1 <- node (Support FixedSupport2D)
   link [Beam 2,Load2D 0 1600,Beam 2] r1 r2
   r2 <- node (Support Pin)
   link [Beam 1,Load2D 0 600,Beam 2] r2 r3
   r3 <- node (Support Roller)
   return ()

example3 = fst $ runInput $ mdo
  r1 <- node (Support (Friction 0.3))
  link [BeamTurn (1/4) ,Beam 2,BeamTurn (-1/4),Beam 3, Load2D 0 500,Beam 3 , BeamTurn (1/4) , BeamTurn (-1/6) ,Load,BeamTurn (-1/12),BeamTurn (-1/4) ,Beam 2,BeamTurn (1/4) ] r1 r2
  r2 <- node (Support (Friction 0.3))
  return ()


example4  = fst $ runInput $ mdo
  x1 <- node (Support FixedSupport3D)
  x2 <- node (Support FixedSupport3D)
  x3 <- node (Support FixedSupport3D )
  x4 <- node (Support FixedSupport3D)
  let h = 2.5
      d = 4.0
      col =  link [BTurn (0.25,0), Beam h ,BTurn (-0.25,0) ]
      vg = [BTurn (0,-0.25),Beam (d/2),Load3D (V3 0 0(-500)) 0,Beam (d/2)]
  link [BTurn (-0.35,0.25), Beam 2 ,BTurn (0.35,0), BTurn (0.5,0),Load3D (V3 0 0 500) 0  ,BTurn (-0.5,0)] x1 x4
  link [BTurn (0.45,0.25), Beam 3 ,BTurn (-0.45,0) ] x4 x2
  link [BTurn (0.15,-0.25), Beam 4 ,BTurn (-0.25,0) ] x4 x3
