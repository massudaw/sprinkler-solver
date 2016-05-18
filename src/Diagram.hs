{-# LANGUAGE MultiParamTypeClasses,ViewPatterns,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Diagram (diagramRender) where

import Grid
import Hydraulic
import qualified Linear.V3 as V3
import Debug.Trace
import Lint
import Rotation.SO3
import Data.Maybe
import Sprinkler
import Tee
import Position
import Element
import Numeric.AD
import qualified Data.Map as M
import Control.Applicative
import qualified Data.List as L
import qualified Data.Set as S
import Data.Ord
import Control.Monad.Trans.State
import Control.Monad
import Data.Foldable (foldMap)
import Data.List (maximumBy,minimumBy)
import Data.Traversable (traverse)
import Diagrams.LinearMap         (amap)
import Linear.Matrix
import Exponential.Class
import Diagrams.ThreeD.Projection
import Diagrams.Prelude hiding (trace,regPoly,offset)
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text (Text)




renderElem s n (Open i) =  regPoly 6 0.1
renderElem s n (Tee (TeeConfig tc@[rl,b,rr] _  _ ) _ ) = regPoly 3 0.1
renderElem s n (Sprinkler ((d,k))  _ _ _) = regPoly 5 0.1
renderElem  _ _ _ = regPoly 4 0.1

renderLinkSVG  _ sl l t@(Tubo _ c _) =   line
  where
    line =  translateX (c/2) ahead <> (fromOffsets [c *^ unitX ] )
renderLinkSVG  _ sl l j@(Joelho _ _ )  =  joelho
  where
    joelho = fromOffsets [0.1 *^ unitX,0.1 *^ unitY]
renderLinkSVG  _ sl l i = mempty

instance RBackend (Path V3 Double) where
  type TCoord (Path V3 Double ) = V3 Double
  errorItem = errorCross
  transformElement (r,x)= translateX (r ^. _x) . translateY (r ^. _y) . translateZ(r ^. _z) . about (unr3 $ unRot123 x)

instance Target  Element (Path  V3 Double ) where
  renderNode = renderElem
  renderLink = renderLinkSVG


errorCross =   fromOffsets [ unitX , unitY  ]

ahead :: Path V3 Double
ahead =   reflectX $ ( (fromOffsets[0.20*unitX]) <>  reflectY (fromOffsets[0.20*unitX] )) -- # lwL 0.04


polyTrail :: OrderedField n => PolygonOpts n -> Located (Trail V3 n)
polyTrail po = transform ori tr
    where
        tr = case po^.polyType of
            PolyPolar ans szs -> polyPolarTrail ans szs
            PolySides ans szs -> polySidesTrail ans szs
            PolyRegular n r   -> polyRegularTrail n r
        ori = case po^.polyOrient of
            OrientH      -> orient unit_Y tr
            OrientV      -> orient unitX  tr
            -- OrientTo v   -> orient v      tr
            NoOrient     -> mempty

polySidesTrail :: OrderedField n =>  [Angle n] -> [n] -> Located (Trail V3 n)
polySidesTrail ans ls = tr `at` (centroid ps # scale (-1))
  where
    ans'    = scanl (^+^) zero ans
    offsets = zipWith (\i j -> transform (aboutZ i ) j) ans' (map (unitY ^*) ls)
    ps      = scanl (.+^) origin offsets
    tr      = closeTrail . trailFromOffsets $ offsets

polyPolarTrail :: OrderedField n =>  [Angle n] -> [n] -> Located (Trail V3  n)
polyPolarTrail [] _ = emptyTrail `at` origin
polyPolarTrail _ [] = emptyTrail `at` origin
polyPolarTrail ans (r:rs) = tr `at` p1
  where
    p1 = unitX  # scale r
    tr = closeTrail . trailFromVertices $
           zipWith
             (\a l -> transform (aboutZ a) . scale l $ unitX )
             (scanl (^+^) zero ans)
             (r:rs)
-- | Generate the vertices of a regular polygon.  See 'PolyRegular'.
polyRegularTrail :: OrderedField n =>  Int -> n -> Located (Trail V3 n)
polyRegularTrail n r = polyPolarTrail
                         (replicate (n - 1) $ fullTurn ^/ fromIntegral n)
                         (repeat r)

leftTurn3 :: (Num n, Ord n) => V3 n -> V3 n -> Bool
leftTurn3 v1 v2 = (v1 `dot` perp v2) < 0
  where perp (V3 a b c ) = V3 (negate b) a c

orient :: OrderedField n => V3 n -> Located (Trail V3 n) -> Transformation V3 n
orient v = orientPoints v . trailVertices

orientPoints :: OrderedField n => V3 n -> [Point V3 n] -> Transformation V3 n
orientPoints v xs = aboutZ a
  where
    (n1,x,n2) = maximumBy (comparing (distAlong v . sndOf3))
                  (zip3 (tail (cycle xs)) xs (last xs : init xs))
    distAlong w ((.-. origin) -> p) = signum (w `dot` p) * norm (project w p)
    sndOf3 (_,b,_) = b
    -- a :: Angle (Scalar v)
    a = minimumBy (comparing $ abs . view rad)
        . map (angleFromNormal . (.-. x)) $ [n1,n2]
    v' = signorm v
    -- angleFromNormal :: v -> Angle (Scalar v)
    angleFromNormal o
      -- | leftTurn3 o' v' = phi
      | otherwise      = negated phi
      where
        o' = signorm o
        theta = acos (v' `dot` o')
        -- phi :: Angle (Scalar v)
        phi
          | theta <= tau/4 = tau/4 - theta @@ rad
          | otherwise      = theta - tau/4 @@ rad


polygon :: (InSpace V3 n t, TrailLike t, OrderedField n) => PolygonOpts n -> t
polygon = trailLike . Diagram.polyTrail


regPoly :: (InSpace V3 n t, TrailLike t, OrderedField n) => Int -> n -> t
regPoly n l = Diagram.polygon (def & polyType .~
                               PolySides
                                 (repeat (1/fromIntegral n @@ turn))
                                 (replicate (n-1) l)
                           & polyOrient .~ OrientH
                           )

diagramRender :: FilePath -> Path V3 Double -> IO ()
diagramRender file house = do
   let
     -- Isometric projection (specialised orthogonal)
     isometricHouse = stroke $ isometricApply zDir house

   renderSVG file (mkSizeSpec2D (Just 800) Nothing) ( hsep 1 . map (sized (mkHeight 3) . centerXY) $ [ isometricHouse ] :: Diagram B)
