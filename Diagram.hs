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
import Language.Mecha.Types
import Language.Mecha.Solid

import Diagrams.Prelude hiding (trace,offset)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text (Text)

instance Coord R2 where
  type Ang R2 = Double



renderElem
  ::
          [Double] -> (S.Set Int, (Double,(Int, Element Double)))
     ->  (Diagram SVG R2)
renderElem _ (s,(p,(n,Open i))) =  text (show n) # fontSizeL 0.1 # fc white <> circle 0.1 # fc green # lwL 0.04
renderElem _ (s,(p,(n,Tee (TeeConfig tc@[rl,b,rr] _ _ _ _)) )) =   text (show n ) # fontSizeL 0.2 # fc black <> rotateBy (1/4) (triangle 0.4) # fc red # lwL 0.04
renderElem [maxf,minf] ite@((s,(p,(n,Sprinkler (Just (d,k))  _ _ _) ))) =  sp
  where
        nf = (k*sqrt p)/(maxf -minf)
        sp = text (show n ) # fontSizeL 0.2 # fc black <> circle 0.2 # opacity (0.3 + 0.7*nf) # fc darkblue # lwL 0.001 <> circle 0.21 #lwL 0.04 <> flabel <> plabel
        flabel = translate (r2 (0,-0.45)) (text (formatFloatN 2 (k*sqrt p) ++ " L/min") # fontSizeL 0.2 )
        plabel = translate (r2 (0,-0.70)) (text (formatFloatN 2 p ++ " kpa") # fontSizeL 0.2 )
renderElem _ i = error $  show i

renderLinkSVG :: (Renderable Text b,
                      Renderable (Path R2) b ) =>
                      (Double,Double)
                      -> Int
                      -> Element Double
                      -> Diagram b R2
renderLinkSVG (f,nf) l t@(Tubo _ c _) =   (line <> foldr1 mappend (zipWith (\i j-> translate (r2 (c/2,(0.4 - 0.2*i ))) j ) [0..] [label , dlabel , llabel , flabel]))
  where
    label  = text (show l) # fontSizeL 0.2 # fc black
    dlabel  = text (show (1000* (fromJust $ diametroE t)) ++ " cm" ) # fontSizeL 0.2 # fc black
    llabel  = text (show (1000*  c ) ++ " cm" ) # fontSizeL 0.2 # fc black
    flabel  = text ((formatFloatN 2 f) ++ " L/min" ) # fontSizeL 0.2 # fc black
    line =  translate (r2 (c/2,0)) (if f > 0 then ahead else reflectX ahead )<> fromOffsets [realToFrac c * unitX ] # lwL 0.04# opacity (0.3 + 0.7*nf) # lc darkblue
renderLinkSVG f _ j@(Joelho _ _ _ _ )  =  joelho
  where
    joelho = circle 0.1 # fc blue # lwL 0.04
renderLinkSVG f _ i = mempty

instance Target  (Diagram SVG R2) where
  renderNode = renderElem
  renderLink = renderLinkSVG
  errorItem = errorCross
  type TCoord (Diagram SVG R2) = R2
  transformElement (r,a) = translate r . rotateBy a


errorCross =  rotateBy (1/8) ( hrule 0.5 <> vrule 0.5 ) # lc red # lwL 0.08

ahead =   reflectX $ (rotateBy (1/8) (fromOffsets[0.20*unitX]) <>  reflectY (rotateBy (1/8) $fromOffsets[0.20*unitX] ))# lwL 0.04

