{-# LANGUAGE RecursiveDo #-}
module Main where
import Thermal
import Domains
import Input
import Project
import Backend.Graphviz
import Grid hiding (var)
import Equation
import Data.GraphViz.Types.Generalised

import Position

testResistor :: Fractional a => Grid Thermal a
testResistor = fst $ runInput $ (mdo
    n1 <- node (Ambient (-10))
    n4 <- node (Ambient 20)
    n2 <- node ThermalNode
    link [Conductor 0.4,Conductor 4.62,Conductor 0.36] n1 n2
    n5 <- node ThermalNode
    link [Conductor 48.48] n2 n5
    link [Conductor 48.48] n2 n5
    link [Conductor 1.01] n2 n5
    link [Conductor 0.36,Conductor 0.16 ] n5 n4
    return ())

main = do
  putStrLn (displayEquation testResistor thermalEq )
  let iter = solveIter (initIter testResistor 0) thermalEq
  print (drawIterGraph testResistor :: [DotStatement Int])
  renderGraph (drawIterGraph  testResistor) "testThermal.png"
  print $ iter
