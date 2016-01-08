{-# LANGUAGE FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Diagram where

import Grid
import Debug.Trace
import Lint
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
import Data.Traversable (traverse)

import Diagrams.Prelude hiding (trace,offset)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text (Text)




{-renderElem
  ::
          [Double] -> (S.Set Int, (Double,(Int, Element Double)))
     ->  (Diagram SVG R2)-}
renderElem _ (s,(p,(n,Open i))) =  hexagon 0.1 -- text (show n) # fontSizeL 0.1 # fc white <> circle 0.1 # fc green # lwL 0.04
renderElem _ (s,(p,(n,Tee (TeeConfig tc@[rl,b,rr] _ _ _ _)) )) =   triangle 0.1 -- text (show n ) # fontSizeL 0.2 # fc black <> rotateBy (1/4) (triangle 0.4) # fc red # lwL 0.04
renderElem [maxf,minf] ite@((s,(p,(n,Sprinkler (Just (d,k))  _ _ _) ))) =  pentagon 0.1 {-sp
  where
        nf = (k*sqrt p)/(maxf -minf)
        sp = text (show n ) # fontSizeL 0.2 # fc black <> circle 0.2 # opacity (0.3 + 0.7*nf) # fc darkblue # lwL 0.001 <> circle 0.21 #lwL 0.04 <> flabel <> plabel
        flabel = translate (r2 (0,-0.45)) (text (formatFloatN 2 (k*sqrt p) ++ " L/min") # fontSizeL 0.2 )
        plabel = translate (r2 (0,-0.70)) (text (formatFloatN 2 p ++ " kpa") # fontSizeL 0.2 )-}
renderElem _ i = error $  show i

{-renderLinkSVG :: (Renderable Text b,
                      Renderable (Path R2) b ) =>
                      (Double,Double)
                      -> Int
                      -> Element Double
                      -> Diagram b R2-}
renderLinkSVG (f,nf) l t@(Tubo _ c _) =   (line ) -- <> foldr1 mappend (zipWith (\i j-> translate (r3 (c/2,(0.4 - 0.2*i ),0)) j ) [0..] [label , dlabel , llabel , flabel]))
  where
    -- label  = text (show l) # fontSizeL 0.2 -- # fc black
    -- dlabel  = text (show (1000* (fromJust $ diametroE t)) ++ " cm" ) # fontSizeL 0.2 -- # fc black
    -- llabel  = text (show (1000*  c ) ++ " cm" ) # fontSizeL 0.2 -- # fc black
    --flabel  = text ((formatFloatN 2 f) ++ " L/min" ) # fontSizeL 0.2 -- # fc black
    line =  translateX (c/2) (if f > 0 then ahead else reflectX ahead )<> (fromOffsets [c *^ unitX ] ) -- # lwL 0.04# opacity (0.3 + 0.7*nf)
renderLinkSVG f _ j@(Joelho _ _ _ _ )  =  joelho
  where
    joelho = fromOffsets [0.1 *^ unitX,0.1 *^ unitY]
renderLinkSVG f _ i = mempty

instance
        Target  (Path  V3 Double ) where
  type TCoord (Path V3 Double ) = V3 Double
  -- renderNode = renderElem
  renderLink = renderLinkSVG
  errorItem = errorCross
  transformElement (r,(ax,ay,az))= translateX (r ^. _x) . translateY (r ^. _y) . translateZ(r ^. _z) . transform (aboutX (realToFrac ax *2*pi @@ rad)) . transform ( aboutZ (realToFrac az *2*pi @@ rad)) . transform (aboutY (realToFrac ay *2*pi @@ rad))


errorCross =   fromOffsets [ unitX , unitY  ]

ahead :: Path V3 Double
ahead =   reflectX $ ( (fromOffsets[0.20*unitX]) <>  reflectY (fromOffsets[0.20*unitX] )) -- # lwL 0.04


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
