{-# LANGUAGE ScopedTypeVariables,MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Backend.DXF (renderDXF) where

import Data.Distributive
import GHC.Stack
import DXF
import Linear.Matrix
import Debug.Trace
import Numeric
import Plane
import Hydraulic
import Force
import Position
import Rotation.SO3
import Exponential.SO3
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

incSeed (Header m s) = (s,Header m (s+1))

addEntity en dxf = dxf { header = nh , entities = e:(entities  dxf)}
  where e = en s
        (s,nh ) = incSeed (header dxf)



{-
testtrig3 = do
  let
    trig = contourBandplotTrig3 [V2 0 5 , V2 5 0 , V2 0 0] [3,8,8] 0 8 0.1 0.01
    quad = contourBandplotquad4 (zip [3,3,3,8] [V2 5 0 ,V2 5 5 ,V2 0 5 , V2 0 0 ] )  0 8 0.1 0.01
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

renderElemMecha  s ni i = error $ show (s,ni,i)

renderLinkMecha  _ nis ni (Tubo (Just d)  c _ ) = (Mecha.color (0.2,0.2,1, 1 ) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d (c*0.9999)) <> Mecha.moveY (d/2) (Mecha.moveX (c/2)(Mecha.scale (0.03,0.03,0.03) $  (Mecha.text (show ni <> "-" <> show nis ))))


renderLinkMecha  _ nis ni (Joelho (Just d)  c _  _  ) = Mecha.sphere d <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha  _ nis ni  (Bomba i  v ) = Mecha.moveX (0.03/2) $ Mecha.sphere 0.4 <> (Mecha.scale (0.03,0.03,0.03) $ Mecha.text (show ni <> "-" <> show nis ))
renderLinkMecha  _ nis _  o = Mecha.sphere 0.02

-}

rotationToAxis (V3 (V3 a11 a12 a13) (V3 a21 a22 a23) (V3 a31 a32 a33)) = (t, V3 (a32- a23) (a13 - a31) (a21 - a12)^/ (2*sin t) )
  where t = acos ((a11 + a22 + a33 -1)/2)

change b a = Position.rotM (V3 0 0 a) !*! (if (b == V3 0 0 1) then identV3 else ( rot2V3 b (V3 0 0 1)))
instance RBackend [EntityTy]  where
  type TCoord [EntityTy] = V3 Double
  transformElement v l = transformEl v <$>  l
    where
      transformEl (v@(V3 mx my mz),SO3 r ) (INSERT n p s ro ax  attr) = (INSERT n ( p ^+^ v) s ro ax attr)
      transformEl (v@(V3 mx my mz),SO3 r ) (LINE o t   ) = (LINE ( o ^+^ v) (v ^+^  r !* t))
      transformEl (v@(V3 mx my mz),SO3 r ) (CIRCLE o ra ) = (CIRCLE( o ^+^ v) ra)
      transformEl (v@(V3 mx my mz),SO3 r ) (TEXT o h t _ _ ) = (TEXT ( {-maybe v (\i -> i !* v) (liftA2  change ax a )-} v ^+^ r !* o  ) h t  (fmap (\i -> 180*i/pi) $ a ) Nothing )
        where (a,ax) = case rotationToAxis $ distribute r of
                         (a ,ax) -> if a == 0 || abs (abs a - pi) < 1e-9 then (Nothing,Nothing) else (Just a , Just $ax )
    -- where v = (\(V3 x y z ) -> V4 x y z (V4 0 0 0 1)) $ liftA2 (\(V3 x y z) m -> V4 x  y z m ) (unSO3 s) r
  statements = concat
instance Target Element [EntityTy] where
  renderLink _ nis ni (Tubo (Just d) c _ ) = [TEXT (V3 (c/2) 0.3 0) 0.2 (show d)  Nothing Nothing, LINE 0 (V3 c 0 0)]
  renderLink _ nis ni (Joelho (Just d) c _ _ ) = [CIRCLE 0 d]
  renderLink _ nis ni i = [CIRCLE 0 0.2]
  renderNode _ nis (Sprinkler _ _ _ _)  = [INSERT "spk" 0  (Just 1) Nothing Nothing []]
  renderNode _ nis i = [CIRCLE 0 0.2]

