{-# LANGUAGE ScopedTypeVariables,MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Backend.Mecha (marrow3d,arrow3d,arrow3dl,module Mecha) where

import GHC.Stack
import Debug.Trace
import Numeric
import Domains
import Rotation.SO3
import Position (rotM)
import Linear.V3
import Linear.V4

import qualified Data.Foldable as F

import Language.Mecha.Types as Mecha
import Language.Mecha.Solid as Mecha
import Language.Mecha.Export as Mecha

import Control.Applicative
import Data.Semigroup

instance RBackend Mecha.Solid where
  type TCoord Mecha.Solid = V3 Double
  errorItem = Mecha.torus 0.2 0.1
  transformElement (r@(V3 mx my mz),s)=  Mecha.affine (F.toList $ fmap F.toList v)
    where v = (\(V3 x y z ) -> V4 x y z (V4 0 0 0 1)) $ liftA2 (\(V3 x y z) m -> V4 x  y z m ) (unSO3 s) r
  statements = Mecha.Statements

marrow3d ni
  | abs ni <1e-9 = Nothing
  | otherwise = Just $ fp $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (ff 3 $abs ni)) <> ( Mecha.rotateZ (-pi/6) $ (Mecha.difference (Mecha.torus  1 0.03) (Mecha.moveY (0.5) $ Mecha.moveX (-0.5) $ Mecha.cube 1))  <> (Mecha.moveY (0.5) $ Mecha.rotateZ pi $  Mecha.rotateY (pi/2) $ Mecha.cone 0.12 0  0.3))
  where fp = if ni < 0 then Mecha.rotateZ pi else id

arrow3dl ni l
  | abs ni <1e-9 = Nothing
  | otherwise = Just $ fp $  ( Mecha.rotateZ pi$  Mecha.moveX (-0.3) $ Mecha.rotateY (pi/2) (Mecha.cone 0.12 0  0.3)) <> Mecha.rotateY (pi/2) ( Mecha.cylinder 0.03 1) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (l)))
  where fp = if ni < 0 then Mecha.rotateZ pi else id


arrow3d ni
  | abs ni <1e-9 = Nothing
  | otherwise = Just $ fp $  ( Mecha.rotateZ pi$  Mecha.moveX (-0.3) $ Mecha.rotateY (pi/2) (Mecha.cone 0.12 0  0.3)) <> Mecha.rotateY (pi/2) ( Mecha.cylinder 0.03 1) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (ff 3 $abs ni)))
  where fp = if ni < 0 then Mecha.rotateZ pi else id


ff numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

instance Semigroup Mecha.Solid where
  i <> j = Mecha.union i j
