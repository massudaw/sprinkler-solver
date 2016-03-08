{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,FlexibleContexts #-}
module Plane where

import Data.Monoid
import Linear.V2
import Domains
import Numeric.AD
import Control.Lens
import Data.Distributive
import Data.Distributive.Generic
import Data.Functor.Classes
import Data.Functor.Compose
import Utils
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.List as L
import Linear.V3
import Debug.Trace
import Control.Applicative
import Data.Complex
import Linear.Matrix
import Linear.Vector
import Linear.Metric

gaussrules :: Floating a => [([a],[a])]
gaussrules
  =  [([0],[2])
     ,([-1,1]^/ sqrt 3,[1,1])
     ,([-sqrt (3/5) ,0 , sqrt $ 3/5],[5/9,8/9,5/9])
     ,([-sqrt((3+2* sqrt(6/5))/7),-sqrt((3-2*sqrt(6/5))/7),  sqrt((3-2*sqrt(6/5))/7), sqrt((3+2*sqrt(6/5))/7)],[1/2 - sqrt (5/6)/6 , 1/2 + sqrt (5/6)/6,1/2 + sqrt (5/6)/6 , 1/2 - sqrt (5/6)/6 ])
     ,((/3) <$> [-sqrt(5+2*sqrt(10/7)),-sqrt(5-2*sqrt(10/7)),0,
 sqrt(5-2*sqrt(10/7)), sqrt(5+2*sqrt(10/7))] ,(/900) <$>  [322-13*sqrt 70,322+13*sqrt 70,512, 322+13*sqrt 70,322-13*sqrt 70])
     ]

instance (Applicative f ,Applicative g ,Additive g  ) => Additive (Compose f  g) where
  zero = Compose  $pure zero
  Compose f ^+^ Compose g  = Compose $ liftA2 (^+^) f g


instance Show1 ZipList where
  showsPrec1 = showsPrec


hexa8stiffness :: (Show a,Floating a) => [V3 a] -> Compose V2 V3 (Compose V2 V3 a) ->  Compose ZipList V3 ( Compose ZipList V3 a)
hexa8stiffness  ncoor  emat
  = foldr1 (!+!) $ map loop  ([[i,j,k] | i<- [0..rule] , j<- [0..rule], k<- [0..rule]])
    where
      rule = 2
      loop [i,j,z] = (*c) **^  (sequenceA be !*! (emat !*!  be))
        where
          -- be :: [Compose [] V2 a]
          be = Compose . ZipList <$> Compose (V2 (V3 ((\x -> (V3 x 0 0 ))  <$> dnx) ( (\y -> V3 0 y 0 ) <$> dny) ( (\z -> V3 0  0 z ) <$> dnz)) (V3 (zipWith (\y x -> V3 y x 0 ) dny  dnx) (zipWith (\z y -> V3 0 z y  ) dnz  dny) (zipWith (\z x  -> V3  z 0  x ) dnz  dnx)))
          c = jdet *  w
          (q,w) = sqrule (V3 rule rule rule) (V3 i j z )
          (jdet,V3 dnx dny dnz) = hexa8isopshape ncoor q



quad4stiffness :: (Show a,Floating a) => [V2 a] -> a -> V3 (V3 a )->  Compose ZipList V2 ( Compose ZipList V2 a)
quad4stiffness  ncoor h emat
  = foldr1 (!+!) $ map loop  ([[i,j] | i<- [0..rule] , j<- [0..rule]])
    where
      rule = 1
      loop [i,j] = (*c) **^  (sequenceA be !*! (emat !*!  be))
        where
          -- be :: [Compose [] V2 a]
          be = Compose . ZipList <$> V3 ((\x -> (V2 x 0))  <$> dnx) ( (\y -> V2 0 y) <$> dny) (zipWith (\y x -> V2 y x) dny  dnx)
          c = jdet * h* w
          (q,w) = sqrule (V2 rule rule) (V2 i j)
          (jdet,V2 dnx dny) = quad4isopshapesimple ncoor q

nodeDir2 :: Num a =>  [V2 a ]
nodeDir2 = [V2 (-1) (-1) , V2 1 (-1) , V2 1 1 , V2 (-1) 1 ]
nodeDir :: Num a =>  [V3 a ]
nodeDir = [V3 (-1) (-1) (-1), V3 1 (-1) (-1), V3 1 1 (-1) , V3 (-1) 1 (-1) , V3 (-1) (-1) 1, V3 1 (-1) 1,V3 1 1  1, V3 (-1) 1 1]

dni :: Fractional a => V3 a -> V3 a -> a
dni (V3 i j k) (V3 xi eta mi)  = (1 + i*xi)*(1 + j*eta)*(1 + k*mi)/8
dni2 ::Fractional  a => V2 a -> V2 a -> a
dni2 (V2 i j ) (V2 xi eta )  = (1 + i*xi)*(1 + j*eta)/4

dnj3 :: Fractional a => V3 a -> [V3 a]
dnj3 = jacobian (\i-> (\j -> dni j i) <$> nodeDir)

dnj2 :: Fractional a => V2 a -> [V2 a]
dnj2 = jacobian (\i-> (\j -> dni2 j i) <$> nodeDir2)

hexa8isopshape :: Fractional a => [V3 a] -> V3 a ->  ( a, V3 [a])
hexa8isopshape coords v@(V3 xi eta mi) = (jdet,(/jdet) **^ dnxyz)
  where
    dj = dnj3 v
    mj =  distribute  coords !*! dj
    jdet = det33 mj
    dnxyz = distribute mj !*! distribute dj


quad4isopshapesimple coords v@(V2 xi eta) = (jdet,(/jdet) **^ dnxy)
  where
    dj = dnj2 v
    mj =  distribute  coords !*! dj
    jdet = det22 mj
    dnxy = distribute mj !*! distribute dj


quad4isopshape coords (V2 xi eta) = (jdet, V2 (dnx ^/ jdet) (dny  ^/ jdet ))
  where
    -- nf =  [(1-xi)*(1-eta), (1+xi)*(1-eta) ,(1+xi)*(1+eta) ,(1-xi)*(1+eta)] ^/ 4
    dnxi = [negate (1-eta),(1-eta),(1+eta),negate (1+eta)] ^/ 4
    dneta = [negate (1-xi),negate (1+xi),(1+xi),(1- xi)] ^/ 4
    x  = (\(V2 x _) -> x) <$> coords
    y  = (\(V2 _ y ) -> y) <$> coords
    j11 = dnxi `dot` x
    j12 = dnxi `dot` y
    j21 = dneta `dot` x
    j22 = dneta `dot` y
    jdet = (j11 * j22) - (j12 * j21)
    dnx = (j22 *^ dnxi) ^-^  (j12 *^ dneta)
    dny = (j11 *^ dneta) ^-^  (j21 *^ dnxi)


sqrule rule point = (fst <$> l , product $ snd <$> l)
    where
      l =  linegauss <$> rule <*> point
      linegauss rule point = (g!! point , w !! point)
        where (g,w) = gaussrules !! rule



test2 = do
    print $ quad4isopshapesimple coord wei
    print emat
  where
    wei = V2 (-1/sqrt 3) 1
    em = 48*63*13*107
    nu = 1/3
    h = 1 :: Double
    a = 9
    coord = [V2  0 0,V2 (2*a) 0,V2 a a,V2 0 a]
    emat = ematQ em nu


ematQ em nu = (*(em/(1 - nu^2))) **^ V3 (V3 1 nu 0 ) (V3 nu 1 0) (V3  0 0 ((1-nu)/2))

test = do
    print $ quad4isopshapesimple coord wei
    print $ quad4isopshape coord wei
    print $ quad4isopshape coord wei == quad4isopshapesimple coord wei
    print k
  where
    wei = V2 (-1/sqrt 3) 1
    k = quad4stiffness coord h emat
    em = 96
    nu = 1/3
    h = 1 :: Double
    a = 9
    coord = [V2 (2*a) 0,V2 (2*a) a,V2 0 a, V2  0 0]
    emat = ematQ em nu

path :: [(Int,Int)] -> [Int]
path im = go (M.fromList im) (head im)
  where
    go m (i,h)
      | M.null m = []
      | otherwise = h: go (M.delete h m) (h,justError ("no tail element of " <> show (i,h) <> show im )$M.lookup h m)

ematT em v =(/(em /((1+v)*(1-2*v))))**^ (m (m (1-v) v v 0 0 0) (m (1-v) v v 0 0 0) (m v v (1-v)  0 0 0) (m 0 0  0 (1/2 - v) 0  0) (m 0 0 0 0 (1/2 -v) 0) (m 0 0 0 0 0 (1/2 -v)))
  where m x y z a b c = Compose  $ V2 (V3 x y z ) (V3 a b c)

