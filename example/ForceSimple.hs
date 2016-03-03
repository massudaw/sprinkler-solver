
{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

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

main = mapM solve $ zip [0..] [example7] -- [example1,example2,example3]
solve (i,ex) = do

  -- putStrLn $ displayEquation ex momentForceEquations
  let ini = makeIter 0 1 ex
      preres = printResidual ini  momentForceEquations
  let iter = solveIter ( makeIter 0 1 ex) momentForceEquations
      posres = printResidual iter momentForceEquations

  putStrLn $ "Pre Resídual: " <>  show preres
  putStrLn $ "Pos Resídual: " <>  show posres
  putStrLn $ "Resídual Improvement: " <>  show (zipWith (/) posres  preres)
  putStrLn $ "Node Forces:" <>  show (printResidual iter forces)
  putStrLn $ "Link Forces:" <>  show (printResidual iter forces)
  displaySolve ("force-model" <> show i ,iter )
  print (pressures iter)
  print (fmap (fst.snd)$  shead $ grid iter)

{-
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
-}


instance Fractional a => Fractional (Maybe a) where
  fromRational i = Just (fromRational i)
instance Num a => Num (Maybe a) where
  fromInteger i = Just (fromInteger i)
  i + j  = liftA2 (+) i  j
  negate  = fmap negate

example5 = fst $ runInput $ mdo
  x2 <- node (Support (Tag (V3 0 0 0 ) 0 ))
  x1 <- node (Support (Tag (V3 Nothing 0 0) 0 ) )
  x3 <- node (Support (Tag (V3 Nothing Nothing 0) (V3 2 (1) 0) ))
  link [aco 10 1] x1 x2
  link [BTurn (0,-3/8),aco (sqrt (10^2 + 10^2)) (2*sqrt 2),BTurn (0,3/8),Load2D 2 1] x2 x3
  link [BTurn (0,-1/4),aco 10 0.5,BTurn (0,1/4)] x3 x1

example6 = fst $ runInput $ mdo
  x2 <- node (Support (Tag (V3 0 (0.5) 0 ) 0 ))
  x1 <- node (Support (Tag (V3 Nothing (-0.4) 0) 0 ) )
  x3 <- node (Support (Tag (V3 Nothing Nothing 0) (V3 2 (1) 0) ))
  link [aco 10 1] x1 x2
  link [BTurn (0,-3/8),aco (sqrt (10^2 + 10^2)) (2*sqrt 2),BTurn (0,3/8),Load2D 2 1] x2 x3
  link [BTurn (0,-1/4),aco 10 0.5,BTurn (0,1/4)] x3 x1


fixed = node (Support (Tag (V3 0 0 0 ) (V3  Nothing Nothing 0 )))
pin = node (Support (Tag (V3 Nothing Nothing 0) 0 ))
roller = node (Support (Tag (V3  Nothing 0 0) (V3 0 Nothing 0) ))
aload load = node (Support (Tag (V3 Nothing Nothing 0) (V3 0 load 0)))
aturn x y =BTurn (0, atan2 x y / (2*pi))

example7 = fst $ runInput $ mdo
  x0 <- fixed
  let atop = 10
      abot = 2
      abat = 3
      adia = 1
      em = 1000
      aco l s = Beam l em s
  link [aco (10.0) abot, BTurn (0,1/2)] x0 x2
  link [aturn (-5) (-10), aco (sqrt 125) atop,aturn (-5) 10] x1 x0
  x1 <- pin
  link [BTurn (0,1/4) ,aco 5 abat ,BTurn (0,1/4)] x2 x1
  x2 <- aload 10
  link [aturn (3) (10),aco  (sqrt 109) atop , aturn (-3) (10),BTurn (0,1/2)] x1 x3
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
  link [aturn (1) (-10),aco  (sqrt 101) atop , aturn (-1) (-10),BTurn (0,1/2)] x7 x5
  x7 <- pin
  link [BTurn (0,1/4) , aco 8 abat ,BTurn (0,1/4)] x8 x7
  link [aco 10 abot , BTurn (0,1/2)] x6 x8
  link [aturn (8) (10), aco (sqrt 164) adia ,aturn (-8) (10)] x6 x7
  x8 <- aload 10
  link [aco (10) abot , BTurn (0,1/2)] x8 x10
  link [aturn (-3) (10),aco  (sqrt 109) atop , aturn (-3) (-10)] x7 x9
  x9 <- pin
  link [aturn (-5) (-10), aco (sqrt 125) adia ,aturn (5) (-10)] x9 x8
  link [BTurn (0,-1/4) , aco 5 abat  ,BTurn (0,-1/4)] x9 x10
  x10 <- aload 10
  link [aturn (-5) (10), aco (sqrt 125) atop,aturn (5) 10,BTurn (0,1/2)] x9 x11
  link [aco (10) abot , BTurn (0,1/2)] x10 x11
  x11 <- roller
  return ()

aco l s = Beam l 100 s



