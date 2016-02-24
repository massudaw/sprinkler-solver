{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Mecha (openSCAD) where

import Grid
import Debug.Trace
import Data.Distributive
import Lint
import Position
import Data.Maybe
import Sprinkler
import Tee
import Rotation.SO3
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
import Language.Mecha.Export


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
renderElemMecha  _ (_,(_,(ni,Reservatorio  i))) = (Mecha.color (1,1,1,1) $ Mecha.sphere 0.5 )<>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha  _ (_,(_,(ni,Tee (TeeConfig _ r i j _ ) _ ))) = (Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha  [maxf,minf] (_,(p,(ni,Sprinkler (Just (d,k)) _ fa@(SPKCoverage sx sy sz (SPKGoods g _ ) ) a ))) = (coloring $ Mecha.sphere 0.15) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni))) -- <> goods
  where
        nf = f /( maxf - minf )
        nfa = (f - coverageArea fa*a)/( maxf - minf )
        f = k*sqrt p
        coloring = if f - coverageArea fa*a > 0 then Mecha.color (0,0,1,1) else  Mecha.color (1,0,0,1)
        goods = Mecha.moveZ (-sz)  (Mecha.scale (0.8*sx,0.8*sy,1.1*g) (Mecha.color (1,0.0,0, 1 ) $ Mecha.cube  1)) <> Mecha.moveZ (-sz) inner
          where
            inner =  Mecha.color (0.2,0.2,1, 1 ) $  Mecha.difference (Mecha.scale (sx,sy,g) (Mecha.cube  1)) (Mecha.scale (0.8*sx,0.8*sy,1.1*g) (Mecha.cube 1))

renderElemMecha  _ i = error $ show i

renderLinkMecha (f,nf)  _ nis ni (Tubo (Just d)  c _ ) = (Mecha.color (0.2,0.2,1, 1 ) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999)) <> Mecha.moveY (d/2) (Mecha.moveX (c/2)(Mecha.scale (0.03,0.03,0.03) $  (Mecha.text (show ni <> "-" <> show nis ))))
-- renderLinkMecha (f,nf)  _ (Tubo (Just d)  c _ ) = Mecha.color (0.2,0.2,1, 0.3 +0.7*nf) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999)

renderLinkMecha _ _ nis ni (Joelho (Just d)  c _  _  ) = Mecha.sphere d <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha _ _ nis ni  (Bomba i  v ) = Mecha.moveX (0.03/2) $ Mecha.sphere 0.4 <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha _ _ nis _  o = Mecha.sphere 0.02

instance RBackend Mecha.Solid where
  type TCoord Mecha.Solid = V3 Double
  errorItem = Mecha.torus 0.2 0.1
  transformElement (r,s)= Mecha.moveX (r ^. _x) . Mecha.moveY (r ^. _y) . Mecha.moveZ (r ^. _z) . Mecha.rotateX ( ax ) . Mecha.rotateZ (az ) . Mecha.rotateY (ay )
    where (V3 ax ay az) = unRot231 . SO3  $  distribute $ unSO3 s


instance Target  Element Mecha.Solid  where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha

instance Semigroup Mecha.Solid where
  i <> j = Mecha.union i j
