{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Diagram4 where

import Grid
import Debug.Trace
import Lint
import Position
import Data.Maybe
import Sprinkler
import Tee
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

import qualified Language.Mecha.Types as Mecha
import qualified Language.Mecha.Solid as Mecha

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text (Text)

import Control.Lens hiding(transform)

import Data.Void

data Diametro = Diametro Double
data Vazao = Vazao Double
data Pressao = Pressao Double

data  Relation c a b
  = Relation  [(Int,Int,c a b)]



renderElemMecha  _ (_,(_,(ni,Open i))) = Mecha.color (0,1,0,1) $ Mecha.sphere 0.1
renderElemMecha  _ (_,(_,(ni,Reservatorio _ _ i))) = (Mecha.color (1,1,1,1) $ Mecha.sphere 0.5 )<>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha  _ (_,(_,(ni,Tee (TeeConfig _ r i j _ ) ))) =( Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
renderElemMecha  _ (_,(_,(ni,Tee (StaticTee _ r i j _ ) ))) =( Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha  [maxf,minf] (_,(p,(ni,Sprinkler (Just (d,k)) _ fa a))) = (coloring $ Mecha.sphere 0.15) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
  where
        nf = f /( maxf - minf )
        nfa = (f - fa*a)/( maxf - minf )
        f = k*sqrt p
        coloring = if f - fa*a > 0 then Mecha.color (0,0,1,0.3 + 0.7*nf) else  Mecha.color (1,0,0,0.3 + 0.7*abs nfa)


renderElemMecha  _ i = error $ show i

renderLinkMecha (f,nf)  _ (Tubo (Just d)  c _ ) = Mecha.color (0.2,0.2,1, 0.3 +0.7*nf) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999)
renderLinkMecha _ _ (Joelho (Just d)  c _  _  ) = Mecha.sphere d
renderLinkMecha _ _  (Bomba i  v [] []) = Mecha.moveX (0.03/2) $ Mecha.sphere 0.4
renderLinkMecha _ _  o = Mecha.sphere 0.02

instance Target  Mecha.Solid  where
  type TCoord Mecha.Solid = V3 Double
  renderNode = renderElemMecha
  renderLink = renderLinkMecha
  errorItem = Mecha.torus 0.2 0.1
  transformElement (r,(ax,ay,az))= Mecha.moveX (r ^. _x) . Mecha.moveY (r ^. _y) . Mecha.moveZ (r ^. _z) . Mecha.rotateX (ax *2*pi) . Mecha.rotateZ (az *2*pi) . Mecha.rotateY (ay *2*pi)

styleNodes :: Iteration Double -> [Mecha.Solid]
styleNodes  it = fmap (\i -> transformElement (var (fst i) (M.fromList (shead $ grid it))) $ renderNode metrics (S.empty ,((abs $ fst (var (fst i) (M.fromList (shead $ grid it))) ^. _z ) *0 + varn (fst i) (M.fromList (nodeHeads it)),i))) (nodesFlow (grid it)) -- (shead (grid it)) (nodeHeads it)
  where metrics = [maximum (snd <$> flows it), minimum (snd <$> flows it)]

styleLinks :: Iteration Double -> [Mecha.Solid]
styleLinks it = concat $ fmap (\(l,_,_,i)  -> zipWith (\m n -> transformElement m $ renderLink (varn l (M.fromList (flows it)),nf (varn l (M.fromList (flows it)))) l n ) (var l (M.fromList $ linksPosition (grid it)) ) i ) (links (grid it))  -- (flows it)
  where [max,min]= [maximum (snd <$> flows it), minimum (snd <$> flows it)]
        nf f =  abs f /(max - min)

drawIter iter = L.foldl1' (<>) $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter

instance Semigroup Mecha.Solid where
  i <> j = Mecha.union i j
