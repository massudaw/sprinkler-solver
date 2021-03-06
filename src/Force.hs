{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Force where

import Backend.Mecha as Mecha
import Control.Applicative
import Data.Functor.Classes
import Control.Arrow
import Control.Lens
import qualified Data.Foldable as F
import Data.Functor.Compose
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import Domains
import GHC.Stack
import Grid
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Numeric.LinearAlgebra (linspace)
import Plane
import Position
import qualified Position as P
import Rotation.SO3 hiding (rotM)
import Utils

data Support a
  = FixedSupport2D
  | Roller
  | Friction a
  | Pin
  | Cable a
  | SmoothSurface
  | BallAndSocket
  | SinglePin
  | Pin3D
  | FixedSupport3D
  deriving (Eq, Ord, Functor, Show)

tag (Support i) = i
tag (Connection _ i) = i

data Force a
  = Support (Forces (Maybe a))
  | Connection [(Int, (a, a, a))] (Forces (Maybe a))
  | FaceLoop
  | Quad4 {em :: M3 a, thickness :: a}
  | Hexa8 {emt :: Compose V2 V3 (Compose V2 V3 a)}
  | Tetra4 {emt :: Compose V2 V3 (Compose V2 V3 a)}
  | Load
  | Link {length :: a}
  | Bar {length :: a, material :: a, section :: a}
  | Beam {length :: a, material :: a, section :: a, inertia :: V3 a, tmodulus :: a}
  | BeamTurn a
  | BTurn (a, a)
  deriving (Eq, Ord, Functor, Show, Show1)

data Forces a = Forces
  { linearDeformation :: V3 a,
    angularDeformation :: V3 a,
    linearForces :: V3 a,
    angularMomemtum :: V3 a
  }
  deriving (Eq, Ord, Foldable, Functor, Traversable, Show)

contourband xyc@[V2 xc1 yc1, V2 xc2 yc2, V2 xc3 yc3] f@[fc1, fc2, fc3] fmin fmax finc eps
  | fmax < fmin || finc < 0 = []
  where
    sv@[(f1, pc1), (f2, pc2), (f3, pc3)] = L.sortBy (comparing fst) $zip f xyc
    nv = min (floor (fmax - fmin + eps) / finc) 1000

trigbandSegment p1@(V2 x1 y1) p2@(V2 x2 y2) p3@(V2 x3 y3) [f1, f2, f3] fv eps
  | fv < f1 = ((p1, p1), -1)
  | fv > f3 = ((p3, p3), 0)
  | fv <= f2 = ((p21, p13), 1)
  | otherwise = ((p32, p13), 1)
  where
    f21 = if f2 - f1 > eps then eps else f2 - f1
    f32 = if f3 - f2 > eps then eps else f3 - f2
    f1' = if f2 - f1 < eps then f1 - eps else f1
    f3' = if f3 - f2 < eps then f2 + eps else f3
    f31 = f3 - f1
    s212 = max 0 (min ((fv - f1) / f21) 1)
    s211 = 1 - s212
    s313 = max 0 (min ((fv - f1) / f31) 1)
    s311 = 1 - s313
    p21 = V2 (x2 * s212 + x1 * s211) (y2 * s212 + y1 * s211)
    p13 = V2 (x1 * s311 + x3 * s313) (y1 * s311 + y3 * s313)
    s323 = max (min ((fv - f2) / f32) 1) 0
    s322 = 1 - s323
    p32 = V2 (x3 * s323 + x2 * s322) (y3 * s323 + y2 * s322)

instance PreSys Force where
  type Enviroment Force = Identity
  type NodeDomain Force = Forces
  type LinkDomain Force = Compose [] Forces
  type SurfaceDomain Force = V3
  lconstrained = Compose . fmap (constr)
    where
      constr Load = Forces (V3 Nothing (Just 0) (Just 0)) 0 (Just <$> 0) 0
      constr i = Forces (Just <$> 0) 0 (Just <$> 0) 0
  constrained (Support f) = f
  constrained (Connection _ f) = f

  --constrained a = (\(a, b) -> Forces a 0 b 0) . constr $ a
  --  where
  --    constr (Support s) =
  --      case s of
  --        FixedSupport2D -> (V3 Nothing Nothing (Just 0), V3 (Just 0) (Just 0) Nothing)
  --        FixedSupport3D -> (V3 Nothing Nothing Nothing, V3 Nothing Nothing Nothing)
  --        SmoothSurface -> (V3 (Just 0) (Just 0) Nothing, V3 (Just 0) (Just 0) (Just 0))
  --        Pin -> (V3 Nothing Nothing (Just 0), V3 (Just 0) (Just 0) (Just 0))
  --        Pin3D -> (V3 Nothing Nothing Nothing, V3 (Just 0) (Just 0) (Just 0))
  --        SinglePin -> (V3 Nothing Nothing Nothing, V3 Nothing Nothing (Just 0))
  --        Roller -> (V3 0 Nothing 0, V3 0 0 0)
  --        Friction x -> (V3 0 Nothing 0, V3 0 0 0)
  --    constr i = (Just <$> 0, Just <$> 0)
  -- postprocess i = M.toList $ printResidual i forces

momentForceEquations :: forall a. (Show a, Ord a, Floating a) => CoordinateGrid V3 a -> Grid Force a -> M.Map Int (LinkDomain Force a) -> M.Map Int (NodeDomain Force a) -> [a]
momentForceEquations =  momentForce 

rotor :: Floating a => V3 a -> V3 a -> V3 a -> M3 a
rotor l0@(V3 x0 y0 z0) l1 l2 = V3 tx ty tz
  where
    tx@(V3 txx txy txz) = (1 / l) *^ n
      where
        l = norm n
    ty = V3 tyx tyy tyz
      where
        tyx = tzy * txz - tzz * txy
        tyy = tzz * txx - tzx * txz
        tyz = tzx * txy - tzy * txx
    tz@(V3 tzx tzy tzz) = (1 / zl) *^ V3 zx zy zz
      where
        zl = norm (V3 zx zy zz)
        zx = dz * y21 - dy * z21
        zy = dx * z21 - dz * x21
        zz = dy * x21 - dx * y21
    n@(V3 x21 y21 z21) = l1 ^-^ l2
    lm@(V3 xm ym zm) = (1 / 2) *^ (l1 ^+^ l2)
    V3 dx dy dz = l0 ^-^ lm

moment l1 l2 b = transpose (rotor (V3 0 1 0) l1 l2) !*! force b !*! rotor (V3 0 1 0) l1 l2

force (Link _) = 0
force (Beam l e a (V3 ix iy iz) g) = V3 (V3 (e * a / l) 0 0) (V3 0 (12 * e * iz / l ^ 3) 0) (V3 0 0 (12 * e * iy / l ^ 3))
force (Bar l e a) = (V3 (V3 n 0 0) (V3 0 0 0) (V3 0 0 0))
  where
    n = e * a / l

bending (Bar _ _ _) = 0
bending (Link _) = 0
bending (Beam l e a (V3 ix iy iz) g) =
  V3
    (V3 0 0 0)
    (V3 0 0 (-6 * e * iz / l ^ 2))
    (V3 0 (6 * e * iy / l ^ 2) 0)

crosstorsor (Bar _ _ _) = 0
crosstorsor (Link _) = 0
crosstorsor (Beam l e a (V3 ix iy iz) g) =
  V3
    (V3 (- g * ix / l) 0 0)
    (V3 0 (2 * e * iy / l) 0)
    (V3 0 0 (2 * e * iz / l))

torsor (Bar _ _ _) = 0
torsor (Link _) = 0
torsor (Beam l e a (V3 ix iy iz) g) =
  V3
    (V3 (g * ix / l) 0 0)
    (V3 0 (4 * e * iy / l) 0)
    (V3 0 0 (4 * e * iz / l))

nodeForces lmap nvars (ix, (s, el)) =
  foldr1 (\(a, b) (c, d) -> (a ^+^ c, b ^+^ d)) $ (\(h, t, resh, rest, a) -> if ix == h then resh else (if ix == t then rest else error "wrong index")) . flip var lmap <$> F.toList s

linkForces p g linkInPre nodesInPre =
  fmap (\(h, t, resh, rest, a) -> (norm resh, norm resh)) <$> M.toList lmap
  where
    lmap = M.fromList $ eqLink nvars <$> links g
    nvars = M.fromList $ fmap (\((ix, i), v) -> (ix, (i, v))) $ zip (M.toList nodesIn) (snd <$> nodesPosition p)
    nodesIn = nodesInPre

bendIter iter@(Iteration r i e g p) =
  Iteration r i e g (p {nodesPosition = editNodes <$> nodesPosition p, linksPosition = editLinks <$> linksPosition p})
  where
    lmap = M.fromList (links (grid iter))
    npmap = M.fromList (nodesPosition p)
    var2 i = fmap (fromMaybe 0) . (\(i, _, _, _) -> i) . var i
    nmap = getCompose <$> M.fromList (pressures iter)
    editNodes (i, (np, nr)) = (i, (np ^+^ d, nr))
      where
        d = var2 i nmap
    editLinks (i, l) = (i, (\(p, r) -> (p ^+^ dh, if norm (dh ^-^ dt) < 1e-3 then r else SO3 $ transpose ratio !*! unSO3 r)) <$> l)
      where
        (h, t, le) = var i lmap
        ratio = bendingRatio (dt ^-^ dh) (ntp ^-^ nhp)
        (nhp, _) = var h npmap
        (ntp, _) = var t npmap
        dh = var2 h nmap
        dt = var2 t nmap

delta di df ni nf r = ((((transpose b !*! r) !* (V3 (norm bl) 0 0)) ^+^ (ni ^+^ di)) ^-^ (nf ^+^ df), bl)
  where
    b = bendingRatio (df ^-^ di) (nf ^-^ ni)
    bl = (nf ^+^ df) ^-^ (ni ^+^ di)

localToGlobal v l = rot2V3 (normalize v) (normalize l)
  where
    normalize a = a ^/ (norm a)

bendingRatio d l
  | norm (cross l (l ^+^ d)) < 1e-6 = identV3
  | otherwise = localToGlobal l (l ^+^ d)

volumeLink nvars npos lmap smap (ls, Tetra4 e) = zip p (getZipList $ getCompose $ (kres !* Compose (ZipList vars)))
  where
    kres = tetrastiffness coords e
    sfs = (\(l, i) -> if l then fst $ var i smap else first neg <$> (fst $ var i smap)) <$> ls
    neg True = False
    neg False = True
    lks = fmap (fmap (flip var lmap)) <$> sfs
    res = fmap (\(b, (h, t, e)) -> if b then (h, t) else (t, h)) <$> lks
    p = L.nub $ concat $ path <$> res
    coords = fmap (\i -> fst $ var i npos) p
    vars = fmap (\i -> linearDeformation $ var i nvars) p
volumeLink nvars npos lmap smap (ls, Hexa8 e) = zip p (getZipList $ getCompose $ kres !* Compose (ZipList vars))
  where
    kres = hexa8stiffness coords e
    sfs = (\(l, i) -> if l then fst $ var i smap else first neg <$> (fst $ var i smap)) <$> ls
    neg True = False
    neg False = True
    lks = fmap (fmap (flip var lmap)) <$> sfs
    res = fmap (\(b, (h, t, e)) -> if b then (h, t) else (t, h)) <$> lks
    p = L.nub $ concat $ path <$> res
    coords = fmap (\i -> fst $ var i npos) p
    vars = fmap (\i -> linearDeformation $ var i nvars) p

surfaceStress nvars npos lmap (ls, Quad4 e h) = zip p (kres vars)
  where
    kres = quad4stress coords e
    lks = fmap (flip var lmap) <$> ls
    res = (\(b, (h, t, e)) -> if b then (h, t) else (t, h)) <$> lks
    p = reverse $ path res
    coords = fmap (\i -> (\(V3 x y _) -> V2 x y) $ fst $ var i npos) p
    vars = fmap (\i -> (\(Forces (V3 x y _) _ _ _) -> V2 x y) $ var i nvars) p

surfaceLink _ _ _ (_, FaceLoop) = []
surfaceLink nvars npos lmap (ls, Quad4 e h) = zip p (getZipList $ getCompose $ kres !* Compose (ZipList vars))
  where
    kres = quad4stiffness coords h e
    lks = fmap (flip var lmap) <$> ls
    res = (\(b, (h, t, e)) -> if b then (h, t) else (t, h)) <$> lks
    p = path res
    coords = fmap (\i -> (\(V3 x y _) -> V2 x y) $ fst $ var i npos) p
    vars = fmap (\i -> (\(Forces (V3 x y _) _ _ _) -> V2 x y) $ var i nvars) p

eqLink nvars (i, (h, t, [el@(Link l)])) = (i, (h, t, (pure 0, pure 0), (pure 0, pure 0), el))
eqLink nvars (i, (h, t, l)) = (i, (h, t, (rtb !* resh, rtb !* mesh), (rtb !* rest, rtb !* mest), el))
  where
    el = justError "no beam" $ L.find isBeam l
    ((Forces fh mhp _ _), (ph, _)) = nvarsEl h
    ((Forces ft mtp _ _), (pt, _)) = nvarsEl t
    pd = force el
    rt = rotor (V3 0 1 0) pt ph
    rtb = transpose rt
    fhl = rt !* fh
    ftl = rt !* ft
    mh = rt !* mhp
    mt = rt !* mtp
    isBeam (Bar _ _ _) = True
    isBeam (Link _) = True
    isBeam (Beam _ _ _ _ _) = True
    isBeam _ = False
    -- Energy Conservation
    bend = bending el
    bendt = transpose bend
    btor = torsor el
    ctor = crosstorsor el
    resh = (pd !* (fhl ^-^ ftl)) ^+^ (bend !* (mh ^+^ mt))
    mesh = ((btor !* mh ^+^ ctor !* mt)) ^+^ (bendt !* (fhl ^-^ ftl))
    rest = (pd !* (ftl ^-^ fhl)) ^-^ (bend !* (mt ^+^ mh))
    mest = (btor !* mt ^+^ ctor !* mh) ^+^ (bendt !* (fhl ^-^ ftl))
    nvarsEl h = var h nvars

forces :: (Show a, Floating a) => CoordinateGrid V3 a -> Grid Force a -> g -> M.Map Int (Forces a) -> M.Map Int (V3 a)
forces p g linkInPre nodesInPre = sfmap
  where
    lmap = M.fromList $ eqLink nvars <$> links g
    nvars = M.fromList $ fmap (\((ix, i), v) -> (ix, (i, v))) $ zip (M.toList nodesIn) (snd <$> nodesPosition p)
    smap = M.fromList $ eqLink nvars <$> links g
    nodesIn = nodesInPre
    sfmap = M.fromListWith (liftA2 (+)) $ concat $ surfaceStress nodesIn (M.fromList $ nodesPosition p) (M.fromList (links g)) . snd <$> surfaces g

instance Show1 Forces where
  liftShowsPrec i _ pre v = showsPrec pre (($"") <$> (i pre <$> v) )

momentForce :: (Show a, Floating a) => CoordinateGrid  V3  a -> Grid Force a -> M.Map Int (Compose [] Forces a) -> M.Map Int (Forces a) -> [a]
momentForce p g linksInPre nodesInPre = concat $ nodeMerge <$> nodesSet g
  where
    nodesIn = nodesInPre
    nvars = M.fromList $ fmap (\((ix, i), v) -> (ix, (i, v))) $ zip (M.toList nodesIn) (snd <$> nodesPosition p)
    l = reverse $ links g
    nodeMerge (ix, (s, el)) = catMaybes . zipWith3 (\i f j -> if isNothing i || isNothing f then Just j else Nothing) (F.toList a <> F.toList aa) (F.toList fv <> F.toList mv) . zipWith (+) (F.toList m <> F.toList ma) . fmap sum . L.transpose $ (linkEls <> sEls <> vEls)
      where
        Forces _ _ m ma = var ix nodesIn
        Forces a aa fv mv = tag el
        linkEls = (\(a, b) -> F.toList a <> F.toList b) . (\(h, t, resh, rest, a) -> if ix == h then resh else (if ix == t then rest else error "wrong index")) . flip var lmap <$> F.toList s
        sEls = maybeToList ((<> replicate 4 0) . F.toList <$> M.lookup ix smap)
        vEls = maybeToList ((<> replicate 3 0) . F.toList <$> M.lookup ix cmap)
    lmap = M.fromList $ eqLink nvars <$> l
    smap = M.fromListWith (liftA2 (+)) $ concat $ surfaceLink nodesIn (M.fromList $ nodesPosition p) (M.fromList l) . snd <$> surfaces g
    cmap = M.fromListWith (liftA2 (+)) $ concat $ volumeLink nodesIn (M.fromList $ nodesPosition p) (M.fromList l) (M.fromList (surfaces g)) . snd <$> volumes g

instance Coord Force V3 where
  thisElement [h, t] (BTurn (x, y)) = (1,) <$> M.fromList [(h, (V3 0 0 0, so3 0)), (t, (V3 0 0 0, so3 (V3 (pi + opi x) (pi + opi y) 0)))]
  thisElement [h, t] (Link i) = (2,) <$> M.fromList [(h, (V3 0 0 0, so3 0)), (t, (V3 i 0 0, so3 (V3 0 0 pi)))]
  thisElement [h, t] (Beam i _ _ _ _) = (2,) <$> M.fromList [(h, (V3 0 0 0, so3 0)), (t, (V3 i 0 0, so3 (V3 0 0 pi)))]
  thisElement [h, t] (Bar l _ _) = (2,) <$> M.fromList [(h, (V3 0 0 0, so3 0)), (t, (V3 l 0 0, so3 (V3 0 0 pi)))]
  thisElement l i = (\(u, m, j) -> (if u /= 0 then 0 else if m /= 0 then 1 else if j /= 0 then 2 else 2, (0, SO3 . P.rotM $ (V3 (opi u) (opi m) (opi j))))) <$> thisF l i
  

{-elemTrans t = (lengthE t , angleE t)
  where
    angleE  = SO3 . P.rotM . opi . angE
      where
        angE (BeamTurn  r  ) = r3 (0,0,r)
        angE (BTurn  (r,c)  ) = r3 (0,r,c)
        angE  i = r3 (0,0,0)
    lengthE (Bar c  m s ) = r3 (c,0,0)
    lengthE (Link c   ) = r3 (c,0,0)
    lengthE (Beam  c  m s _ _ ) = r3 (c,0,0)
    lengthE i = 0
    r3 (x,y,z) = V3 x y z
    -}

skew231 (V3 x y z) = V3 (V3 0 z (- y)) (V3 (- z) 0 x) (V3 y (- x) 0)

rot2V3 x y = identV3 !+! skewV3 v !+! ((* ((1 - dot x y) / (norm v) ^ 2)) **^ (skewV3 v !*! skewV3 v))
  where
    v = cross x y

thisF l (Connection i _) = M.fromList i
thisF l e = M.fromList (fmap (fmap (0,0,)) . els . (l,) $ e)
  where
    els ([a, b, c, d, e, f], i) =
      [(a, 0), (b, 0), (c, 0), (d, 0), (e, 0), (f, 0)]
    els ([a, b, c, d, e], i) =
      [(a, 0), (b, 0), (c, 0), (d, 0), (e, 0)]
    els ([a, b, c, d], i) =
      [(a, 0), (b, 0), (c, 0), (d, 0)]
    els ([a, b, c], i) =
      [(a, 0), (b, 0), (c, 0)]
    els ([a, b], i) =
      [(a, 0), (b, 0)]
    els ([a], i) =
      [(a, 0)]

v1, v2 :: V3 Double
v1 = V3 1 2 0
v2 = V3 1 2.1 1

--- Test Link reflection

rel = [(Bar 1.0 0 0), BTurn (1 / 4, -1 / 4), Bar 1 0 0, BTurn (1 / 9, 0), Bar 2 0 0] :: [Force Double]

rori = (V3 1 (-1) (1.2 :: Double), SO3 $rotM (0 :: V3 Double))

---

-- testtrig = trigbandsegment [V2 1 2 , V2 0 0 , V2 1 0] [0,2,3] 1 0.4
testtrig3 = do
  let trig = contourBandplotTrig3 [V2 0 5, V2 5 0, V2 0 0] [3, 8, 8] 0 8 0.1 0.01
      quad = contourBandplotquad4 (zip [3, 3, 3, 8] [V2 5 0, V2 5 5, V2 0 5, V2 0 0]) 0 8 0.1 0.01
  writeFile "test.scad" $ T.unpack $ openSCAD $ Statements $ fmap (\(c, p) -> color c (extrude p 0.1)) quad

contourBandplotquad4 xyc@[p1, p2, p3, p4] fmin fmax finc eps =
  concat $ ftab <$> xytab
  where
    p5 = (sum (fmap fst xyc) / 4, V2 (sum $ fmap (^. _2 . _x) xyc) (sum $ fmap (^. _2 . _y) xyc) / 4)
    xytab = [[p1, p2, p5], [p2, p3, p5], [p3, p4, p5], [p4, p1, p5]]
    ftab l = contourBandplotTrig3 b a fmin fmax finc eps
      where
        (a, b) = unzip l
contourBandplotquad4 i _ _ _ _ = error $ show (i, L.length i)

trigbandsegment :: (Show a, Ord a, Fractional a) => [V2 a] -> [a] -> a -> a -> ((V2 a, V2 a), Int)
trigbandsegment [p1@(V2 x1 y1), p2@(V2 x2 y2), p3@(V2 x3 y3)] fs@[f1', f2, f3'] fv eps
  | fv < f1' = ((p1, p1), -1)
  | fv > f3' = ((p3, p3), 0)
  | fv <= f2 = ((p21, p13), 1)
  | otherwise = ((p32, p13), 2)
  where
    df21 = f2 - f1'
    df32 = f3' - f2
    (f1, f21) = if df21 < eps then (f1' - eps, eps) else (f1', df21)
    (f3, f32) = if df32 < eps then (f2 + eps, eps) else (f3', df32)
    f31 = f3 - f1
    s212 = lim ((fv - f1) / f21)
    s211 = 1 - s212
    s313 = lim ((fv - f1) / f31)
    s311 = 1 - s313
    lim i = max 0 (min 1 i)
    p21 = V2 (x2 * s212 + x1 * s211) (y2 * s212 + y1 * s211)
    p13 = V2 (x1 * s311 + x3 * s313) (y1 * s311 + y3 * s313)
    s323 = lim ((fv - f2) / f32)
    s322 = 1 - s323
    p32 = V2 (x3 * s323 + x2 * s322) (y3 * s323 + y2 * s322)

contourBandColor :: (Fractional b, Num d, Ord b, Num c) => b -> b -> b -> (b, b, c, d)
contourBandColor f fmin fmax
  | fmax == 0 || fmin >= fmax = (1, 2, 1, 1)
  | f > fmax || f < fmin = (1, 0, 0, 1)
  | otherwise = (h, s, b, 1)
  where
    h = 0.7 * fs
    b = 1
    fs = (fmax - f) / (fmax - fmin)
    s = 1 - 1 / (1 + 100 * (fs - 1 / 2) ^ 2)

contourBandplotTrig3 xyc@[x1, x2, x3] fc fmin fmax finc eps =
  catMaybes $ reverse $ snd $ foldl loop ((-1, (V2 0 0, V2 0 0), 0), []) gen
  where
    nv = min (floor $ (fmax - fmin + eps) / finc) 1000
    s :: [(Double, V2 Double)]
    s@[(f1, pc1), (f2, pc2), (f3, pc3)] = L.sortBy (comparing fst) $ zip fc xyc
    ptab q1 q2 q3 q4 =
      [[Nothing, Just [pc1, pc2, pc3], Just [pc1, q4, q3], Just [pc1, pc2, q3, q4]], [Nothing, Nothing, Nothing, Nothing], [Nothing, Just [pc3, pc2, q1, q2], Just [q1, q2, q4, q3], Just [pc2, q1, q2, q4, q3]], [Nothing, Just [pc3, q1, q2], Nothing, Just [q1, q2, q4, q3]]]
    flast = fmin - 1000
    kb = 0
    k1 = 0
    fbot iv = fmin + (fromIntegral iv - 1) * finc
    ftop iv = fmin + fromIntegral iv * finc
    gen :: [(Double, Double)]
    gen = [(fbot iv, ftop iv) | iv <- [1 .. nv], not $ fbot iv >= f3 || ftop iv <= f1]
    loop ((flast, plast, tlast), l) (fbot, ftop) =
      ((ftop, pt, tt), ((\p -> (contourBandColor favg fmin fmax, Polygon (fmap (\(V2 x y) -> [x, y]) p) [[0 .. L.length p - 1]])) <$> p) : l)
      where
        favg = (fbot + ftop) / 2
        (pb, tb)
          | fbot == flast = (plast, tlast)
          | otherwise = trigbandsegment (snd <$> s) [f1, f2, f3] fbot eps
        (pt, tt) = trigbandsegment (snd <$> s) [f1, f2, f3] ftop eps
        (p1, p2) = pb
        (p3, p4) = pt
        p = ptab p1 p2 p3 p4 !! (tb + 1) !! (tt + 1)

fromOnly i = maybe i (i <>)

axis i@(V3 x y z) = (scale (is, is, is) <$> arrow3dl x "x") <> (scale (js, js, js) . rotateZ (pi / 2) <$> arrow3dl y "y") <> (scale (ls, ls, ls) . rotateY (pi / 2) <$> arrow3dl z "z")
  where
    is = x / ni
    js = y / ni
    ls = z / ni
    ni = norm i

instance Target Force Solid where
  renderNode ni (Support _) = color (0, 1, 0, 1) $ sphere 0.1 <> (scale (0.03, 0.03, 0.03) (text (show ni)))
  renderNode ni _ = color (0, 1, 0, 1) $ sphere 0.1 <> (moveY 0.2 $ scale (0.03, 0.03, 0.03) (text (show ni))) -- <> fromJust (axis (V3 1 1 1))
  renderNodeSolve (Forces (V3 _ _ _) _ i@(V3 x y z) m@(V3 mx my mz)) ix _ =
    moveZ 2 $ color (0, 1, 0, 1) $ fromOnly (moveY 0.2 $ scale (0.03, 0.03, 0.03) (text (show ix))) $
      (scale (is, is, is) <$> arrow3d x) <> (scale (js, js, js) . rotateZ (pi / 2) <$> arrow3d y) <> (scale (ls, ls, ls) . rotateY (pi / 2) <$> arrow3d z) <> (scale (mzs, mzs, mzs) <$> marrow3d mz <> (scale (mys, mys, mys) . rotateY (pi / 2) <$> marrow3d my) <> (scale (mxs, mxs, mxs) . rotateX (pi / 2) <$> marrow3d mx))
    where
      is = x / ni
      js = y / ni
      ls = z / ni
      ni = norm i
      mzs = mz / norm m
      mys = my / norm m
      mxs = mx / norm m

  renderLink _ nis ni (Link i) = color (0.2, 0.2, 1, 1) $(rotateY (pi / 2) $ cylinder d (abs $ i * 0.99)) <> (move (i / 2, d / 2, d / 2) $ scale (st, st, st) (text (show (ni))))
    where
      d = 0.03 -- 2* (sqrt$ a/pi)
      st = 0.03
  renderLink _ nis ni (Link i) = color (0.2, 0.2, 1, 1) $(rotateY (pi / 2) $ cylinder d (abs $ i * 0.99)) <> (move (i / 2, d / 2, d / 2) $ scale (st, st, st) (text (show (ni))))
    where
      d = 0.03 -- 2* (sqrt$ a/pi)
      st = 0.03
  renderLink _ nis ni (Bar i _ a) = color (0.2, 0.2, 1, 1) $(rotateY (pi / 2) $ cylinder d (abs $ i * 0.99)) <> (move (i / 2, d / 2, d / 2) $ scale (st, st, st) (text (show ni)))
    where
      d = 2 * (sqrt $ a / pi)
      st = 0.09
  renderLink _ nis ni (Beam i _ a _ _) = color (0.2, 0.2, 1, 1) $((moveX (i / 2) $ scale (i, sqrt a, sqrt a) (cube 1))) <> (move (i / 2, d / 2, d / 2) $ scale (st, st, st) (text (show ni)))
    where
      d = 0.03 -- 2* (sqrt$ a/pi)
      st = 0.09
  renderLink _ nis ni (BeamTurn _) = sphere d
    where
      d = 0.03
  renderLink _ nis ni (BTurn _) = sphere d
    where
      d = 0.03
  renderLink _ nis ni (Load) = color (0, 1, 0, 1) $ (rotateZ (pi) $ moveX (-0.3) $ rotateY (pi / 2) (cone 0.12 0 0.3)) <> rotateY (pi / 2) (cylinder 0.03 1) <> (moveY 0.2 $ scale (0.03, 0.03, 0.03) (text (show (ni, nis))))
  renderLinkSolve _ _ _ _  _= sphere 0.01 
  renderSurface ls nds (FaceLoop) = sphere 0.01
  renderSurface ls nds (Quad4 _ _) = sphere 0.01 --  color (0,1,1,0.1) $  extrude ( polygon (F.toList <$> npos) [paths])  0.1
    where
      nls = M.fromList $ zip (fst <$> nds) [0 ..]
      npos = (fst . snd <$> nds)
      paths = fmap (\n -> fromJust $ M.lookup n nls) $ path $ (\(b, (h, t, l)) -> if b then (h, t) else (t, h)) <$> ls
  renderVolume ls nds _ = color (0, 1, 1, 1) $ polyhedra (F.toList <$> npos) paths
    where
      nls = M.fromList $ zip (fst <$> nds) [0 ..]
      npos = (fst . snd <$> nds)
      paths = (fmap (\n -> fromJust $ M.lookup n nls) . path . fmap (\(b, (h, t, l)) -> if b then (h, t) else (t, h))) <$> ls
  renderSurfaceSolve v ls nds (Quad4 _ _) si = st (contourBandplotquad4 (zip px ((\(V3 a b c) -> V2 a b) <$> npos)) (L.minimum px) (L.maximum px) ((L.maximum px - L.minimum px) / 50) 0.01)
    where
      px = (^. _x) <$> (fmap ((\i -> fromJust $ M.lookup i (M.fromList v)) . fst) $ L.nub nds)
      st quad = foldl Union si (fmap (\(c, p) -> color c (extrude p 0.11)) quad)
      nls = M.fromList $ zip (fst <$> L.nub nds) [0 ..]
      npos = (fst . snd <$> L.nub nds)
  renderSurfaceSolve v ls nds (FaceLoop) si = si
