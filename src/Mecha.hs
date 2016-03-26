{-# LANGUAGE ScopedTypeVariables,MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Mecha (openSCAD) where

import Data.Distributive
import GHC.Stack
import Linear.Matrix
import Debug.Trace
import Numeric
import Plane
import Hydraulic
import Force
import Position
import Rotation.SO3
import Data.Maybe
import Linear.V3
import qualified Data.List as L
import Data.Ord
import Linear.V4
import Element
import qualified Data.Foldable as F

import qualified Data.Map as M
import qualified Language.Mecha.Types as Mecha
import qualified Language.Mecha.Solid as Mecha
import Language.Mecha.Export
import qualified Data.Text as T


import Diagrams.Prelude


-- testtrig = trigbandsegment [V2 1 2 , V2 0 0 , V2 1 0] [0,2,3] 1 0.4
testtrig3 = do
  let
    trig = contourBandplotTrig3 [V2 0 5 , V2 5 0 , V2 0 0] [3,8,8] 0 8 0.1 0.01
    quad = contourBandplotquad4 (zip [3,3,3,8] [V2 5 0 ,  V2 5 5 ,V2 0 5 , V2 0 0 ] )  0 8 0.1 0.01
  writeFile "test.scad" $ T.unpack $ openSCAD $ Mecha.Statements $ fmap  (\(c,p)-> Mecha.color c (Mecha.extrude p 0.1 )) quad

contourBandplotquad4 xyc@[p1,p2,p3,p4] fmin fmax finc eps
  = concat $ ftab <$> xytab
  where
    p5 = (sum (fmap fst xyc) /4, V2 (sum $ fmap (^. _2. _x) xyc) (sum $ fmap (^. _2 . _y) xyc) /4)
    xytab = [[p1,p2,p5],[p2,p3,p5],[p3,p4,p5],[p4,p1,p5]]
    ftab l =  contourBandplotTrig3   b a fmin fmax finc eps
      where (a,b ) = unzip l
contourBandplotquad4 i  _ _ _ _ = errorWithStackTrace $ show (i, L.length i)


trigbandsegment :: (Show a ,Ord a ,Fractional a )=> [V2 a ] -> [a] -> a -> a -> ((V2 a,V2 a ),Int)
trigbandsegment [p1@(V2 x1 y1), p2@(V2 x2 y2) , p3@(V2 x3 y3)] fs@[f1',f2,f3'] fv eps
  | fv < f1' = ((p1,p1),-1)
  | fv > f3'  = ((p3,p3),0)
  | fv <= f2 = ((p21,p13),1)
  | otherwise = ((p32,p13),2)
  where
    df21 = f2 - f1'
    df32 = f3' - f2
    (f1,f21) = if df21 < eps then (f1' - eps,eps) else (f1',df21)
    (f3,f32) = if df32 < eps then (f2 +eps,eps )else (f3',df32)
    f31 = f3 - f1
    s212 =  lim ((fv - f1)/f21 )
    s211 = 1 - s212
    s313 = lim ((fv - f1)/f31 )
    s311 = 1 - s313
    lim i= max 0 (min 1 i)
    p21 =  V2 (x2*s212 + x1 *s211) (y2*s212 + y1*s211)
    p13 = V2 (x1*s311 + x3 *s313) (y1*s311 + y3*s313)
    s323 = lim ((fv - f2 )/f32)
    s322 = 1 - s323
    p32 = V2 (x3*s323 + x2*s322) (y3*s323 + y2*s322)


contourBandColor f fmin fmax
  | fmax == 0 || fmin >= fmax = (1,2,1,1)
  | f > fmax || f< fmin = (1,0,0,1)
  | otherwise = (h,s,b,1)
    where
      h = 0.7*fs
      b = 1
      fs  = (fmax - f)/(fmax - fmin)
      s = 1 - 1 /(1 + 100* (fs - 1/2)^2)

contourBandplotTrig3  xyc@[x1,x2,x3] fc fmin fmax finc eps
  = catMaybes $ reverse $ snd $ foldl loop  ((-1, (V2 0 0,V2 0  0),0),[]) gen
  where
    nv = min (floor $ (fmax - fmin + eps)/finc) 1000
    s :: [(Double,V2 Double)]
    s@[(f1,pc1),(f2,pc2),(f3,pc3)] = L.sortBy (comparing fst) $ zip fc xyc
    ptab q1 q2 q3 q4
      = [[Nothing,Just [pc1,pc2,pc3],Just [pc1,q4,q3], Just [pc1,pc2,q3,q4]],[Nothing,Nothing,Nothing,Nothing],[Nothing,Just [pc3,pc2,q1,q2],Just [q1,q2,q4,q3],Just [pc2,q1,q2,q4,q3]],[Nothing,Just [pc3,q1,q2],Nothing,Just [q1,q2,q4,q3]]]
    flast = fmin - 1000
    kb = 0
    k1 = 0
    fbot iv =  fmin + (fromIntegral iv - 1) * finc
    ftop iv = fmin + fromIntegral iv * finc
    gen :: [(Double,Double)]
    gen = [(fbot iv  ,ftop iv ) | iv <- [1..nv  ], not $ fbot iv >= f3 ||  ftop iv <= f1 ]
    loop ((flast,plast ,tlast),l) (fbot,ftop)
      = ((ftop,pt,tt),((\p -> (contourBandColor favg fmin fmax,Mecha.Polygon (fmap (\(V2 x y ) -> [x,y])p) [[0..L.length p - 1]] )) <$> p):l)
      where
        favg = (fbot + ftop)/2
        (pb,tb)
          | fbot == flast = (plast,tlast)
          | otherwise =   trigbandsegment (snd <$> s) [f1,f2,f3] fbot eps
        (pt,tt) = trigbandsegment (snd <$> s) [f1,f2,f3] ftop eps
        (p1,p2) = pb
        (p3,p4) = pt
        p =  ptab p1 p2 p3 p4 !! (tb + 1) !! (tt+ 1)





renderElemMecha  _ ni (Open i) = Mecha.color (0,1,0,1) $ Mecha.sphere 0.1
renderElemMecha  _ ni (Reservatorio  i) = (Mecha.color (1,1,1,1) $ Mecha.sphere 0.5 )<>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

renderElemMecha  _ ni (Tee (TeeConfig _ r _ i j _ ) _ ) = (Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)) <>  (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))

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
  transformElement (r@(V3 mx my mz),s)=  Mecha.affine (F.toList $ fmap F.toList v)
    where v = (\(V3 x y z ) -> V4 x y z (V4 0 0 0 1)) $ liftA2 (\(V3 x y z) m -> V4 x  y z m ) (unSO3 s) r
  statements = Mecha.Statements

fromOnly i = maybe i (i <>)

axis i@(V3 x y z) = ( Mecha.scale  (is,is,is) <$> arrow3dl x "x" )<> (Mecha.scale  (js,js,js) . Mecha.rotateZ (pi/2) <$>  arrow3dl y "y")<> (Mecha.scale  (ls,ls,ls) . Mecha.rotateY (pi/2) <$>arrow3dl z "z")
    where is = x/ni
          js = y/ni
          ls = z/ni
          ni = norm i

instance Target Force Mecha.Solid  where
  renderNode  _ ni (Support (Tag _ _ _ _ )) =   Mecha.color (0,1,0,1) $ Mecha.sphere 0.1 <> (Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))
  renderNode  _ ni _ =  Mecha.color (0,1,0,1) $ Mecha.sphere 0.1 <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ni)))   -- <> fromJust (axis (V3 1 1 1))
  renderNodeSolve (Forces (V3 _ _ _,_,i@(V3 x  y z),m@(V3 mx my  mz))) ix _
    = Mecha.moveZ 2 $  Mecha.color (0,1,0,1) $  fromOnly (Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show ix ))) $
          ( Mecha.scale  (is,is,is) <$> arrow3d x )<> (Mecha.scale  (js,js,js) . Mecha.rotateZ (pi/2) <$>  arrow3d y)<> (Mecha.scale  (ls,ls,ls) . Mecha.rotateY (pi/2) <$>arrow3d z) <> ( Mecha.scale (mzs,mzs,mzs) <$> Mecha.marrow3d mz <> (Mecha.scale  (mys,mys,mys) . Mecha.rotateY (pi/2) <$>marrow3d my ) <> (Mecha.scale  (mxs,mxs,mxs) . Mecha.rotateX (pi/2) <$>  marrow3d mx))
    where is = x/ni
          js = y/ni
          ls = z/ni
          ni = norm i
          mzs = mz/norm m
          mys = my/norm m
          mxs = mx/norm m

  renderLink h nis  ni  (Link i   )  =  Mecha.color (0.2,0.2,1, 1 ) $( Mecha.rotateY (pi/2) $ Mecha.cylinder d (abs $ i*0.99)) <> ( Mecha.move (i/2,d/2,d/2) $ Mecha.scale (st,st,st) (Mecha.text (show (ni,h))))
    where d = 0.03 -- 2* (sqrt$ a/pi)
          st = 0.03
  renderLink _ nis  ni  (Bar i _ a )  =  Mecha.color (0.2,0.2,1, 1 ) $( Mecha.rotateY (pi/2) $ Mecha.cylinder d (abs $ i*0.99)) <> ( Mecha.move (i/2,d/2,d/2)$ Mecha.scale (st,st,st) (Mecha.text (show ni)))
    where d = 2* (sqrt$ a/pi)
          st = 0.09
  renderLink _ nis  ni  (Beam i _ a _ _ )  =  Mecha.color (0.2,0.2,1, 1 ) $(   (Mecha.moveX (i/2) $ Mecha.scale (i,sqrt a , sqrt a) (Mecha.cube 1)  ) )<> ( Mecha.move (i/2,d/2,d/2) $ Mecha.scale (st,st,st) (Mecha.text (show ni)))
    where d = 0.03 -- 2* (sqrt$ a/pi)
          st = 0.09
  renderLink  _  nis ni (BeamTurn _  ) = Mecha.sphere d
    where d = 0.03
  renderLink  _  nis ni (BTurn _  ) = Mecha.sphere d
    where d = 0.03
  renderLink  _ nis ni (Load  ) =  Mecha.color (0,1,0,1) $  (Mecha.rotateZ (pi) $ Mecha.moveX (-0.3) $ Mecha.rotateY (pi/2) (Mecha.cone 0.12 0  0.3)) <> Mecha.rotateY (pi/2) ( Mecha.cylinder 0.03 1) <>  ( Mecha.moveY 0.2 $ Mecha.scale (0.03,0.03,0.03) (Mecha.text (show (ni,nis))))
  renderSurface ls nds (FaceLoop ) =  Mecha.sphere 0.01
  renderSurface ls nds (Quad4 _ _) = Mecha.sphere 0.01 -- Mecha.color (0,1,1,0.1) $ Mecha.extrude (Mecha.polygon (F.toList <$> npos) [paths])  0.1
    where
      nls = M.fromList $ zip (fst <$> nds) [0..]
      npos = (fst . snd <$> nds)
      paths = fmap (\n -> fromJust $M.lookup n nls) $ path $ (\(b,(h,t,l))-> if b then (h,t) else (t,h)) <$> ls
  renderVolume ls nds _ = Mecha.color (0,1,1,1) $Mecha.polyhedra (F.toList <$> npos) paths
    where
      nls = M.fromList $ zip (fst <$> nds) [0..]
      npos = (fst . snd <$> nds)
      paths = (fmap (\n -> fromJust $M.lookup n nls) . path . fmap (\(b,(h,t,l))-> if b then (h,t) else (t,h))  ) <$> ls
  renderSurfaceSolve v ls nds (Quad4 _ _) si  =  st (contourBandplotquad4 (zip px ((\(V3 a b c) -> V2 a b) <$> npos)) (L.minimum px) (L.maximum px) ((L.maximum px - L.minimum px)/50) 0.01)
    where
      px = (^._x) <$> (fmap ((\i -> fromJust  $ M.lookup i (M.fromList v)).fst) $ L.nub nds )
      st quad= foldl Mecha.Union si (fmap (\(c,p)-> Mecha.color c (Mecha.extrude p 0.11 ))   quad)
      nls = M.fromList $ zip (fst <$> L.nub nds) [0..]
      npos = (fst . snd <$> L.nub nds)
      paths = fmap (\n -> fromJust $M.lookup n nls) $ path $ (\(b,(h,t,l))-> if b then (h,t) else (t,h)) <$> ls
  renderSurfaceSolve v ls nds (FaceLoop ) si  =  si





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

instance Target Element Mecha.Solid  where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha

ff numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

instance Semigroup Mecha.Solid where
  i <> j = Mecha.union i j
