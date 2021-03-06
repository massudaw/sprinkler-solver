{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Plane where

import Control.Applicative
import Data.Complex
import Data.Distributive
import qualified Data.Foldable as F
import Data.Functor.Classes
import Data.Functor.Compose
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ord
import Domains
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Numeric.AD
import qualified Numeric.LinearAlgebra as HM
import Utils

gaussrules :: Floating a => [([a], [a])]
gaussrules =
  [ ([0], [2]),
    ([-1, 1] ^/ sqrt 3, [1, 1]),
    ([- sqrt (3 / 5), 0, sqrt $ 3 / 5], [5 / 9, 8 / 9, 5 / 9]),
    ([- sqrt ((3 + 2 * sqrt (6 / 5)) / 7), - sqrt ((3 -2 * sqrt (6 / 5)) / 7), sqrt ((3 -2 * sqrt (6 / 5)) / 7), sqrt ((3 + 2 * sqrt (6 / 5)) / 7)], [1 / 2 - sqrt (5 / 6) / 6, 1 / 2 + sqrt (5 / 6) / 6, 1 / 2 + sqrt (5 / 6) / 6, 1 / 2 - sqrt (5 / 6) / 6]),
    ( (/ 3)
        <$> [ - sqrt (5 + 2 * sqrt (10 / 7)),
              - sqrt (5 -2 * sqrt (10 / 7)),
              0,
              sqrt (5 -2 * sqrt (10 / 7)),
              sqrt (5 + 2 * sqrt (10 / 7))
            ],
      (/ 900) <$> [322 -13 * sqrt 70, 322 + 13 * sqrt 70, 512, 322 + 13 * sqrt 70, 322 -13 * sqrt 70]
    )
  ]

deriving instance Show1 ZipList

hexa8stiffness :: (Show a, Floating a) => [V3 a] -> Compose V2 V3 (Compose V2 V3 a) -> Compose ZipList V3 (Compose ZipList V3 a)
hexa8stiffness ncoor emat =
  foldr1 (!+!) $ map loop ([[i, j, k] | i <- [0 .. rule], j <- [0 .. rule], k <- [0 .. rule]])
  where
    rule = 1
    loop [i, j, z] = (* c) **^ (sequenceA be !*! (emat !*! be))
      where
        -- be :: [Compose [] V2 a]
        be = Compose . ZipList <$> Compose (V2 (V3 ((\x -> (V3 x 0 0)) <$> dnx) ((\y -> V3 0 y 0) <$> dny) ((\z -> V3 0 0 z) <$> dnz)) (V3 (zipWith (\y x -> V3 y x 0) dny dnx) (zipWith (\z y -> V3 0 z y) dnz dny) (zipWith (\z x -> V3 z 0 x) dnz dnx)))
        c = jdet * w
        (q, w) = sqrule (V3 rule rule rule) (V3 i j z)
        (jdet, V3 dnx dny dnz) = hexa8isopshape ncoor q

nodeDir :: Num a => [V3 a]
nodeDir = [V3 (-1) (-1) (-1), V3 1 (-1) (-1), V3 1 1 (-1), V3 (-1) 1 (-1), V3 (-1) (-1) 1, V3 1 (-1) 1, V3 1 1 1, V3 (-1) 1 1]

nodeDirTetra :: Num a => [V3 a]
nodeDirTetra = [V3 (-1) (-1) (-1), V3 1 (-1) (-1), V3 1 1 (-1), V3 (-1) 1 (-1), V3 (-1) (-1) 1, V3 1 (-1) 1, V3 1 1 1, V3 (-1) 1 1]

dni :: Fractional a => V3 a -> V3 a -> a
dni (V3 i j k) (V3 xi eta mi) = (1 + i * xi) * (1 + j * eta) * (1 + k * mi) / 8

dnj3 :: Fractional a => V3 a -> [V3 a]
dnj3 = jacobian (\i -> (\j -> dni j i) <$> nodeDir)

dndctetra4 :: Num a => V4 a -> V4 (V4 a)
dndctetra4 v = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

tetraisopshape :: (Fractional a) => V4 (V3 a) -> V4 a -> (a, V3 (V4 a))
tetraisopshape coords v@(V4 s1 s2 s3 s4) = (jdet, dn)
  where
    dj = dndctetra4 v
    mj = distribute coords !*! dj
    mja = (\(V3 x y z) -> V4 1 x y z) mj
    jdet = det44 mja
    jinv = fmap (\(V4 _ x y z) -> V3 x y z) $ inv44 mja
    dn = (distribute jinv !*! distribute dj)

tetrastiffness :: (Show a, Floating a) => [V3 a] -> Compose V2 V3 (Compose V2 V3 a) -> Compose ZipList V3 (Compose ZipList V3 a)
tetrastiffness coords emat = change $ fmap change $ loop
  where
    change (Compose (V4 a b c d)) = Compose $ ZipList [a, b, c, d]
    unchange ([a, b, c, d]) = (V4 a b c d)
    loop = (* c) **^ (sequenceA be !*! (emat !*! be))
    be = Compose <$> Compose (V2 (V3 ((\x -> (V3 x 0 0)) <$> dnx) ((\y -> V3 0 y 0) <$> dny) ((\z -> V3 0 0 z) <$> dnz)) (V3 (liftA2 (\y x -> V3 y x 0) dny dnx) (liftA2 (\z y -> V3 0 z y) dnz dny) (liftA2 (\z x -> V3 z 0 x) dnz dnx)))
    c = jdet / 6
    (jdet, V3 dnx dny dnz) = tetraisopshape (unchange coords) (V4 1 1 1 1)

testtetra = do
  print s
  where
    p :: V4 (V3 Double)
    p = (V4 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) (V3 1 1 1))
    s = tetraisopshape p (V4 1 1 1 1)

hexa8isopshape :: Floating a => [V3 a] -> V3 a -> (a, V3 [a])
hexa8isopshape coords v@(V3 xi eta mi) = (jdet, dn)
  where
    dj = dnj3 v
    mj = distribute coords !*! dj
    jdet = det33 mj
    dn = inv33 mj !*! distribute dj

--
-- Quad9
--

quad9directions :: Num a => [V2 a]
quad9directions = [V2 (-1) (-1), V2 1 (-1), V2 1 1, V2 (-1) 1, V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0, V2 0 0]

quad9shapefunction :: Fractional a => V2 a -> [a]
quad9shapefunction (V2 s n) =
  [ (1 - s) * s * (1 - n) * n / 4,
    - (1 + s) * s * (1 - n) * n / 4,
    (1 + s) * s * (1 + n) * n / 4,
    - (1 - s) * s * (1 + n) * n / 4,
    - (1 - s ^ 2) * (1 - n) * n / 2,
    (1 + s) * s * (1 - n ^ 2) / 2,
    (1 - s ^ 2) * (1 + n) * n / 2,
    - (1 - s) * s * (1 - n ^ 2) / 2,
    (1 - s ^ 2) * (1 - n ^ 2)
  ]

quad9isopshapesimple ::
  (Show b, Floating b) => [V2 b] -> V2 b -> (b, V2 [b])
quad9isopshapesimple coords v@(V2 xi eta) = (jdet, dn)
  where
    dj = jacobian quad9shapefunction v
    mj = distribute coords !*! dj
    jdet = det22 mj
    dn = distribute (inv22 mj) !*! distribute dj

quad9stiffness :: (Show a, Floating a) => [V2 a] -> a -> V3 (V3 a) -> Compose ZipList V2 (Compose ZipList V2 a)
quad9stiffness ncoor h emat =
  foldr1 (!+!) $ map loop ([[i, j] | i <- [0 .. rule], j <- [0 .. rule]])
  where
    rule = 1
    loop [i, j] = (* c) **^ (sequenceA be !*! (emat !*! be))
      where
        be = Compose . ZipList <$> V3 ((\x -> (V2 x 0)) <$> dnx) ((\y -> V2 0 y) <$> dny) (zipWith (\y x -> V2 y x) dny dnx)
        c = jdet * h * w
        (q, w) = sqrule (V2 rule rule) (V2 i j)
        (jdet, V2 dnx dny) = quad9isopshapesimple ncoor q

quad9stress ::
  (Show a, Floating a) =>
  [V2 a] ->
  V3 (V3 a) ->
  [V2 a] ->
  [V3 a]
quad9stress coord emat udis = loop <$> quad9directions
  where
    loop q = emat !* (be !* (Compose $ ZipList udis))
      where
        (jdet, V2 dnx dny) = quad9isopshapesimple coord q
        be = Compose . ZipList <$> V3 ((\x -> (V2 x 0)) <$> dnx) ((\y -> V2 0 y) <$> dny) (zipWith (\y x -> V2 y x) dny dnx)

quad4directions :: Num a => [V2 a]
quad4directions = [V2 (-1) (-1), V2 1 (-1), V2 1 1, V2 (-1) 1]

quad4shapefunction i = (\j -> dni2 j i) <$> quad4directions

dni2 :: Fractional a => V2 a -> V2 a -> a
dni2 (V2 i j) (V2 xi eta) = (1 + i * xi) * (1 + j * eta) / 4

quad4isopshapesimple coords v@(V2 xi eta) = (jdet, dn)
  where
    dj = jacobian quad4shapefunction v
    mj = distribute coords !*! dj
    jdet = det22 mj
    dn = distribute (inv22 mj) !*! distribute dj

quad4isopshape coords (V2 xi eta) = (jdet, V2 (dnx ^/ jdet) (dny ^/ jdet))
  where
    -- nf =  [(1-xi)*(1-eta), (1+xi)*(1-eta) ,(1+xi)*(1+eta) ,(1-xi)*(1+eta)] ^/ 4
    dnxi = [negate (1 - eta), (1 - eta), (1 + eta), negate (1 + eta)] ^/ 4
    dneta = [negate (1 - xi), negate (1 + xi), (1 + xi), (1 - xi)] ^/ 4
    x = (\(V2 x _) -> x) <$> coords
    y = (\(V2 _ y) -> y) <$> coords
    j11 = dnxi `dot` x
    j12 = dnxi `dot` y
    j21 = dneta `dot` x
    j22 = dneta `dot` y
    jdet = (j11 * j22) - (j12 * j21)
    dnx = (j22 *^ dnxi) ^-^ (j12 *^ dneta)
    dny = (j11 *^ dneta) ^-^ (j21 *^ dnxi)

quad4stress ::
  (Floating a) =>
  [V2 a] ->
  V3 (V3 a) ->
  [V2 a] ->
  [V3 a]
quad4stress coord emat udis = loop <$> quad4directions
  where
    loop q = emat !* (be !* (Compose $ ZipList udis))
      where
        (jdet, V2 dnx dny) = quad4isopshapesimple coord q
        be = Compose . ZipList <$> V3 ((\x -> (V2 x 0)) <$> dnx) ((\y -> V2 0 y) <$> dny) (zipWith (\y x -> V2 y x) dny dnx)

quad4distributedforce coord h bfor =
  foldr1 (!+!) $ map loop ([V2 i j | i <- [0 .. rule], j <- [0 .. rule]])
  where
    rule = 1
    loop v = (* c) **^ bk
      where
        bk = ZipList $ (*^ br) <$> nf
        c = jdet * h * w
        br = nf *! bfor
        (q, w) = sqrule (V2 rule rule) v
        (jdet, _) = quad4isopshapesimple coord q
        nf = quad4shapefunction q

quad4stiffness :: (Show a, Floating a) => [V2 a] -> a -> V3 (V3 a) -> Compose ZipList V2 (Compose ZipList V2 a)
quad4stiffness ncoor h emat =
  foldr1 (!+!) $ map loop ([[i, j] | i <- [0 .. rule], j <- [0 .. rule]])
  where
    rule = 1
    loop [i, j] = (* c) **^ (sequenceA be !*! (emat !*! be))
      where
        be = Compose . ZipList <$> V3 ((\x -> (V2 x 0)) <$> dnx) ((\y -> V2 0 y) <$> dny) (zipWith (\y x -> V2 y x) dny dnx)
        c = jdet * h * w
        (q, w) = sqrule (V2 rule rule) (V2 i j)
        (jdet, V2 dnx dny) = quad4isopshapesimple ncoor q

sqrule rule point = (fst <$> l, product $ snd <$> l)
  where
    l = linegauss <$> rule <*> point
    linegauss rule point = (g !! point, w !! point)
      where
        (g, w) = gaussrules !! rule

{-
test4 = do
  {-  ClearAll[Em,ν]; Em=96; ν=1/3;
Emat=Em/((1+ν)*(1-2*ν))*{{1-ν,ν,ν,0,0,0},
 {ν,1-ν,ν,0,0,0},{ν,ν,1-ν,0,0,0},{0,0,0,1/2-ν,0,0},
 {0,0,0,0,1/2-ν,0},{0,0,0,0,0, 1/2-ν}};
Print["Emat=",Emat//MatrixForm];
ncoor={{2,3,4},{6,3,2},{2,5,1},{4,3,6}};-}

    print coord
    print $ k
    print $L.sortBy (flip $comparing abs) $ fmap realPart $ HM.toList $  HM.eigenvalues (HM.fromLists $ F.toList $ fmap F.toList k)
  where
    k = tetrastiffness coord emat
    em = 480
    nu = 1/3
    coord = [ V3 2 3 4 , V3 6 3 2 , V3 2 5 1 , V3 4 3 6]
    emat = ematT em nu
-}

test3 = do
  print $ k
  print $ L.sortBy (flip $comparing abs) $ fmap realPart $ HM.toList $ HM.eigenvalues (HM.fromLists $ F.toList $ fmap F.toList k)
  where
    k :: Compose ZipList V3 (Compose ZipList V3 Double)
    k = hexa8stiffness coord emat
    em = 480
    nu = 1 / 3
    a = 8
    -- nodeDir = [V3 (-1) (-1) (-1), V3 1 (-1) (-1), V3 1 1 (-1) , V3 (-1) 1 (-1) , V3 (-1) (-1) 1, V3 1 (-1) 1,V3 1 1  1, V3 (-1) 1 1]
    coord = [V3 0 0 0, V3 a 0 0, V3 a a 0, V3 0 a 0, V3 0 0 a, V3 a 0 a, V3 a a a, V3 0 a a]
    emat = ematT em nu

test2 = do
  print $ k
  print $fmap realPart $ HM.toList $ HM.eigenvalues (HM.fromLists $ F.toList $ fmap F.toList k)
  where
    k :: Compose ZipList V2 (Compose ZipList V2 Double)
    k = quad4stiffness coord h emat
    wei = V2 (-1 / sqrt 3) 1
    em = 48 * 63 * 13 * 107
    nu = 1 / 3
    h = 1 :: Double
    a = 9
    coord = [V2 0 0, V2 (2 * a) 0, V2 a a, V2 0 a]
    emat = ematQ em nu

ematQ em nu = (* (em / (1 - nu ^ 2))) **^ V3 (V3 1 nu 0) (V3 nu 1 0) (V3 0 0 ((1 - nu) / 2))

testq9 = do
  let h = 1
      a = 2
      b = 1
      ncoor :: [V2 Double]
      ncoor = [V2 0 0, V2 a 0, V2 a b, V2 0 b, V2 (a / 2) 0, V2 a (b / 2), V2 (a / 2) b, V2 0 (b / 2), V2 (a / 2) (b / 2)]
      em = 96 * 39 * 11 * 55 * 7
      nu = 1 / 3
      emat = ematQ em nu
      ke = quad9stiffness ncoor h emat
      q = V2 (1 / sqrt 3) (-1 / sqrt 3)
      sh = quad9isopshapesimple ncoor q
      vb = quad9shapefunction q
  print emat
  print sh
  print vb
  print ke

  print $ L.sortBy (flip $comparing abs) $ fmap realPart $ HM.toList $ HM.eigenvalues (HM.fromLists $ F.toList $ fmap F.toList ke)

test = do
  print $ quad4isopshapesimple coord wei
  print $ quad4isopshape coord wei
  print $ quad4isopshape coord wei == quad4isopshapesimple coord wei

  print $ k
  print $ quad4distributedforce coord 1 bf
  print $ quad4stress coord emat sf
  where
    wei = V2 (-1 / sqrt 3) 1
    k = quad4stiffness coord h emat
    em = 96
    bf = [V2 1 1, V2 1 1, V2 1 1, V2 1 1]
    sf = [V2 0 1, V2 1 0, V2 1 1, V2 1 0]
    nu = 1 / 3
    h = 1 :: Double
    a = 9
    coord = [V2 (2 * a) 0, V2 (2 * a) a, V2 0 a, V2 0 0]
    emat = ematQ em nu

path :: [(Int, Int)] -> [Int]
path im = go (M.fromList im) (head im)
  where
    go m (i, h)
      | M.null m = []
      | otherwise = h : go (M.delete h m) (h, justError ("no tail element of " <> show (i, h) <> show im) $ M.lookup h m)

{-
Emat=Em/((1+ν)*(1-2*ν))*{{1-ν,ν,ν,0,0,0},
 {ν,1-ν,ν,0,0,0},{ν,ν,1-ν,0,0,0},{0,0,0,1/2-ν,0,0},
  {0,0,0,0,1/2-ν,0},{0,0,0,0,0, 1/2-ν}};
-}
ematT em v = (* (em / ((1 + v) * (1 -2 * v)))) **^ (m (m (1 - v) v v 0 0 0) (m v (1 - v) v 0 0 0) (m v v (1 - v) 0 0 0) (m 0 0 0 (1 / 2 - v) 0 0) (m 0 0 0 0 (1 / 2 - v) 0) (m 0 0 0 0 0 (1 / 2 - v)))
  where
    m x y z a b c = Compose $ V2 (V3 x y z) (V3 a b c)
