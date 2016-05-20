{-# LANGUAGE ScopedTypeVariables,MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Backend.DXF (renderDXF,module DXF) where

import Data.Distributive
import GHC.Stack
import DXF
import Linear.Matrix
import Debug.Trace
import Numeric
import Hydraulic
import Domains
import Rotation.SO3
import Exponential.SO3
import Data.Maybe
import Linear.V3
import qualified Data.List as L
import Data.Ord
import Linear.V4
import qualified Data.Foldable as F

import qualified Data.Map as M
import qualified Data.Text as T

import Linear.Metric
import Linear.Matrix
import Linear.Vector
import Data.Semigroup



genEntity ty@(LINE x y) l h = Entity "LINE" (Object h "4C9" "AcDbEntity"  Nothing l Nothing Nothing Nothing Nothing (Just "AcDbLine"))  ty
genEntity ty@(INSERT _ _ _ _ _ _ ) l h = Entity "INSERT" (Object h "1F" "AcDbEntity"  Nothing l Nothing Nothing Nothing Nothing (Just "AcDbBlockReference"))  ty
genEntity ty@(CIRCLE x y)l  h = Entity "CIRCLE" (Object h "1F" "AcDbEntity" Nothing l Nothing Nothing Nothing Nothing (Just "AcDbCircle"))  ty
genEntity ty@(TEXT _ _ _ _ _ )l  h = Entity "TEXT" (Object h "1F" "AcDbEntity" Nothing l Nothing Nothing Nothing Nothing (Just "AcDbText"))  ty


renderDXF :: FilePath -> FilePath -> [EntityTy] -> IO ()
renderDXF b f o = do
  dxfe <- readDXF (b <> ".DXF")
  dxf <- case  dxfe of
           Right v -> return v
           Left e -> error e

  let genPath o = (\s -> genEntity  o "grid" s )
  writeDXF (f <> "-OUT.DXF") $ (foldr addEntity dxf (genPath <$> o))
  return ()



arbritaryAxis n@(V3 nx ny nz)  = V3 nax  ( normalize (n `cross` nax)) n
  where
    wz = V3 0 0 1
    wy = V3 0 1 0
    nax = normalize ax
    ax
      | abs nx < 1e-12 && abs ny <1e-12 = wy `cross` n
      | otherwise  = wz `cross` n

{-
If (Nx < 1/64) and (Ny < 1/64) then
       Ax = Wy * N      (where "*" is the cross-product operator).
Otherwise,
       Ax = Wz * N.
Scale Ax to unit length.

The method of getting the Ay vector would be:

    Ay = N * Ax.
Scale Ay to unit length.
-}


rotationToAxis (V3 (V3 a11 a12 a13) (V3 a21 a22 a23) (V3 a31 a32 a33)) = (t, V3 (a32- a23) (a13 - a31) (a21 - a12)^/ (2*sin t) )
  where t = acos ((a11 + a22 + a33 -1)/2)


testConv = (filter (not.(<= 1e-6 ). norm .(^-^ V3 10  9 2) . unconv ) $  conv <$> (concat $ sym <$> angleSet ))
  where
    angleSet = [pi/2,pi/3,pi/6]
    sym a = angle  a <> fmap negate (angle a)
    angle a = [(V3  a 0  0),(V3 0 a 0), (V3 0 0 a),(V3 a a 0),V3 0 a a,V3 a 0 a]
    conv a = convert2 (V3 10 9 2, SO3 $ rotM a)
    unconv = unconvert2

unconvert2 (ax,p) =  distribute tr !* p
    where tr = arbritaryAxis ax

convert2 :: (V3 Double,SO3 Double) -> (V3 Double ,V3 Double)
convert2 (p,SO3 r)  = ( nz,   arbritaryAxis nz !* p  )
  where nz = r !* V3 0 0 1



instance RBackend [EntityTy]  where
  type TCoord [EntityTy] = V3 Double
  transformElement v l = transformEl v <$>  l
    where
      transformEl (v,SO3 r ) (INSERT n p s ro ax  attr) = (INSERT n ( p ^+^ v) s ro ax attr)
      transformEl (v,SO3 r ) (LINE o t   ) = (LINE ( o ^+^ v) (v ^+^  r !* t))
      transformEl (v,SO3 r ) (CIRCLE o ra ) = (CIRCLE( o ^+^ v) ra)
      transformEl or@(v,SO3 r) (TEXT o h t _ _ ) = (TEXT np h t  (fmap (\i -> 180*i/pi) $ if abs a < 1e-5 then Nothing else Just  a) (if norm (ax - (V3 0 0 1) ) < 1e-10  then  Nothing else Just ax) )
        where (ax,np) = convert2 ( v ^+^ r !* o , SO3 r)
              (a,_ ) = rotationToAxis r
  statements = concat


