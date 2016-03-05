{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Mecha (openSCAD) where

import Data.Distributive
import Numeric
import Hydraulic
import Force
import Position
import Rotation.SO3
import Linear.V3
import Element

import qualified Language.Mecha.Types as Mecha
import qualified Language.Mecha.Solid as Mecha
import Language.Mecha.Export


import Diagrams.Prelude





renderElemMecha  _ ni (Open i) = Mecha.color (0,1,0,1) $ Mecha.sphere 0.1
renderElemMecha  _ ni (Reservatorio  i) = (Mecha.color (1,1,1,1) $ Mecha.sphere 0.5 )<>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha  _ ni (Tee (TeeConfig _ r i j _ ) _ ) = (Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha  _ ni (Sprinkler (Just (d,k)) _ fa@(SPKCoverage sx sy sz (SPKGoods g _ ) ) a ) = (Mecha.sphere 0.15) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

{-renderElemMecha  [maxf,minf] (_,(p,(ni,Sprinkler (Just (d,k)) _ fa@(SPKCoverage sx sy sz (SPKGoods g _ ) ) a ))) = (coloring $ Mecha.sphere 0.15) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni))) -- <> goods
  where
        nf = f /( maxf - minf )
        nfa = (f - coverageArea fa*a)/( maxf - minf )
        f = k*sqrt p
        coloring = if f - coverageArea fa*a > 0 then Mecha.color (0,0,1,1) else  Mecha.color (1,0,0,1)
        goods = Mecha.moveZ (-sz)  (Mecha.scale (0.8*sx,0.8*sy,1.1*g) (Mecha.color (1,0.0,0, 1 ) $ Mecha.cube  1)) <> Mecha.moveZ (-sz) inner
          where
            inner =  Mecha.color (0.2,0.2,1, 1 ) $  Mecha.difference (Mecha.scale (sx,sy,g) (Mecha.cube  1)) (Mecha.scale (0.8*sx,0.8*sy,1.1*g) (Mecha.cube 1))
-}
renderElemMecha  s ni i = error $ show (s,ni,i)

renderLinkMecha  _ nis ni (Tubo (Just d)  c _ ) = (Mecha.color (0.2,0.2,1, 1 ) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999)) <> Mecha.moveY (d/2) (Mecha.moveX (c/2)(Mecha.scale (0.03,0.03,0.03) $  (Mecha.text (show ni <> "-" <> show nis ))))
-- renderLinkMecha (f,nf)  _ nis ni (Tubo (Just d)  c _ ) = (Mecha.color (0.2,0.2,1, 1 ) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999)) <> Mecha.moveY (d/2) (Mecha.moveX (c/2)(Mecha.scale (0.03,0.03,0.03) $  (Mecha.text (show ni <> "-" <> show nis ))))
-- renderLinkMecha (f,nf)  _ (Tubo (Just d)  c _ ) = Mecha.color (0.2,0.2,1, 0.3 +0.7*nf) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999)

renderLinkMecha  _ nis ni (Joelho (Just d)  c _  _  ) = Mecha.sphere d <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha  _ nis ni  (Bomba i  v ) = Mecha.moveX (0.03/2) $ Mecha.sphere 0.4 <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha  _ nis _  o = Mecha.sphere 0.02

instance RBackend Mecha.Solid where
  type TCoord Mecha.Solid = V3 Double
  errorItem = Mecha.torus 0.2 0.1
  transformElement (r,s)= Mecha.moveX (r ^. _x) . Mecha.moveY (r ^. _y) . Mecha.moveZ (r ^. _z) . Mecha.rotateX ( ax ) . Mecha.rotateZ (az ) . Mecha.rotateY (ay )
    where (V3 ax ay az) = unRot231 . SO3  $  distribute $ unSO3 s

fromOnly i = maybe i (i <>)

instance Target Force Mecha.Solid  where
  renderNode  _ ni (Support (Tag _ _ _ _ )) =   Mecha.color (0,1,0,1) $ Mecha.sphere 1
  renderNode  _ ni _ =  Mecha.color (0,1,0,1) $ Mecha.sphere 1 <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
  renderNodeSolve (Forces (V3 _ _ _,_,i@(V3 x  y z),m@(V3 mx my  mz))) ix _
    = Mecha.moveZ 2 $  Mecha.color (0,1,0,1) $ Mecha.scale (2,2,2) $ fromOnly (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ix ))) $
          ( Mecha.scale  (is,is,is) <$> arrow3d x )<> (Mecha.scale  (js,js,js) . Mecha.rotateZ (pi/2) <$>  arrow3d y)<> (Mecha.scale  (ls,ls,ls) . Mecha.rotateY (pi/2) <$>arrow3d z) <> ( Mecha.scale (mzs,mzs,mzs) <$> Mecha.marrow3d mz <> (Mecha.scale  (mys,mys,mys) . Mecha.rotateY (pi/2) <$>marrow3d my ) <> (Mecha.scale  (mxs,mxs,mxs) . Mecha.rotateX (pi/2) <$>  marrow3d mx))
    where is = x/ni
          js = y/ni
          ls = z/ni
          ni = norm i
          mzs = mz/norm m
          mys = my/norm m
          mxs = mx/norm m

  renderLink _ nis  ni  (Bar i _ a )  =  Mecha.color (0.2,0.2,1, 1 ) $( Mecha.rotateY (pi/2) $ Mecha.cylinder d (abs $ i*0.99)) <> ( Mecha.moveY (d/2) $Mecha.moveZ (d/2)  $ Mecha.moveX (i/2)$ Mecha.scale (st,st,st) (Mecha.text (show ni)))
    where d = 0.03 -- 2* (sqrt$ a/pi)
          st = 0.09
  renderLink _ nis  ni  (Beam i _ a _ _ )  =  Mecha.color (0.2,0.2,1, 1 ) $(   (Mecha.moveX (i/2) $ Mecha.scale (i,sqrt a , sqrt a) (Mecha.cube 1)  ) )<> ( Mecha.moveY (d/2) $Mecha.moveZ (d/2)  $ Mecha.moveX (i/2)$ Mecha.scale (st,st,st) (Mecha.text (show ni)))
    where d = 0.03 -- 2* (sqrt$ a/pi)
          st = 0.09
  renderLink  _  nis ni (BeamTurn _  ) = Mecha.sphere d
    where d = 0.03
  renderLink  _  nis ni (BTurn _  ) = Mecha.sphere d
    where d = 0.03
  renderLink  _ nis ni (Load  ) =  Mecha.color (0,1,0,1) $  (Mecha.rotateZ (pi) $ Mecha.moveX (-0.3) $ Mecha.rotateY (pi/2) (Mecha.cone 0.12 0  0.3)) <> Mecha.rotateY (pi/2) ( Mecha.cylinder 0.03 1) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show (ni,nis))))

marrow3d ni
  | abs ni <1e-9 = Nothing
  | otherwise = Just $ fp $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (ff 3 $abs ni)) <> ( Mecha.rotateZ (-pi/6) $ (Mecha.difference (Mecha.torus  1 0.03) (Mecha.moveY (0.5) $ Mecha.moveX (-0.5) $ Mecha.cube 1))  <> (Mecha.moveY (0.5) $ Mecha.rotateZ pi $  Mecha.rotateY (pi/2) $ Mecha.cone 0.12 0  0.3))
  where fp = if ni < 0 then Mecha.rotateZ pi else id

arrow3d ni
  | abs ni <1e-9 = Nothing
  | otherwise = Just $ fp $  ( Mecha.rotateZ pi$  Mecha.moveX (-0.3) $ Mecha.rotateY (pi/2) (Mecha.cone 0.12 0  0.3)) <> Mecha.rotateY (pi/2) ( Mecha.cylinder 0.03 1) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (ff 3 $abs ni)))
  where fp = if ni < 0 then Mecha.rotateZ pi else id

instance Target Element Mecha.Solid  where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha

ff numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

instance Semigroup Mecha.Solid where
  i <> j = Mecha.union i j
