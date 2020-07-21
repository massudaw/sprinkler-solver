{-# LANGUAGE RecursiveDo #-}
module Main where
import Eletric
import Domains
import Input
import Project
import Backend.Graphviz
import Grid hiding (var)
import Equation

import Position

testResistor :: Fractional a => Grid Eletric a
testResistor = fst $ runInput $ (mdo
    n1 <- node Ground
    link [Resistor 4] n1 n2
    n2 <- node Node
    n5 <- node Node
    link [Resistor 2] n2 n5
    link [VoltageSource 10] n5 n1
    n3 <- node Node
    link [Resistor 5] n1 n3
    link [Resistor 2.5] n2 n3
    n4 <- node Node
    link [Resistor 1 ] n1 n4
    link [VoltageSource 4] n4 n3
    return ())

main = do
  putStrLn (displayEquation testResistor circuitEq)
  let iter = solveIter (initIter testResistor 0) circuitEq
  print $ iter
  renderGraph (drawIterGraph  testResistor ) "testResistor.png"
