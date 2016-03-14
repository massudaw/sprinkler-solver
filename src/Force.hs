{-# LANGUAGE TupleSections,MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable #-}
module Force where

import Utils
import Position
import Plane
import Control.Applicative
import qualified Position as P
import Data.Maybe
import Domains
import Data.Monoid
import Linear.V4
import Data.Functor.Compose
import Data.Functor.Classes
import Linear.Metric
import Linear.V3
import Linear.V2
import Linear.Matrix
import Linear.Vector
import Control.Arrow
import Rotation.SO3 hiding (rotM)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Foldable as F
import Debug.Trace
import Numeric.GSL.ODE
import Numeric.LinearAlgebra (linspace)

xdot t [x,v] = [v, -0.95*x - 0.1*v]

ts = linspace 100 (0,20 :: Double)

sol = odeSolve xdot [10,0] ts


data Support a
  = FixedSupport2D
  | Roller
  | Friction a
  | Pin
  | Tag  (V3 (Maybe a)) (V3 (Maybe a)) (V3 (Maybe a)) (V3 (Maybe a))
  | Cable a
  | SmoothSurface
  | BallAndSocket
  | SinglePin
  | Pin3D
  | FixedSupport3D
  deriving(Eq,Ord,Functor,Show)

tag (Support i) = i
tag (Connection _ i) = i

data Force a
  = Support  (Support a)
  | Connection [(Int,(a,a,a))] (Support a)
  | FaceLoop
  | Quad4 { em :: M3 a, thickness :: a }
  | Hexa8 { emt :: Compose V2 V3 (Compose V2 V3 a)}
  | Tetra4 { emt :: Compose V2 V3 (Compose V2 V3 a)}
  | Load
  | Link {length :: a}
  | Bar { length :: a, material  :: a  , section :: a }
  | Beam { length :: a, material  :: a  , section :: a ,  inertia :: V3 a , tmodulus  :: a  }
  | BeamTurn a
  | BTurn (a,a)
  deriving(Eq,Ord,Functor,Show)

newtype Forces a = Forces {unForces :: (V3 a , V3 a , V3 a, V3 a )} deriving (Foldable,Functor,Traversable,Show)

instance Fractional a => Fractional (Maybe a) where
  fromRational i = Just (fromRational i)
instance Num a => Num (Maybe a) where
  fromInteger i = Just (fromInteger i)
  i + j  = liftA2 (+) i  j
  negate  = fmap negate

instance Show1 Forces where
  showsPrec1 = showsPrec

instance PreSys Force where
  type NodeDomain Force = Forces
  type LinkDomain Force = Compose  [] Forces
  revElem (BeamTurn i )  = (BeamTurn (-i))
  revElem (BTurn (a,b) )  = (BTurn (a,-b))
  revElem i = i
  lconstrained = Compose . fmap (Forces . constr)
    where
    constr Load = (V3 Nothing (Just 0)  (Just 0) ,0,Just <$> 0,0)
    constr i  = (Just <$> 0,0,Just <$> 0,0)
  constrained (Support (Tag t a l m) ) = Forces (t,a,l,m)
  constrained (Connection _ (Tag t a l m) ) = Forces (t,a,l,m)
  constrained a =  Forces . (\(a,b) -> (a,0,b,0) ) . constr $ a
    where
    constr (Support s ) =
      case s of
        FixedSupport2D -> (V3 Nothing Nothing (Just 0) ,V3 (Just 0) (Just 0) Nothing)
        FixedSupport3D -> (V3 Nothing Nothing Nothing ,V3 Nothing Nothing Nothing)
        SmoothSurface -> (V3 (Just 0) (Just 0) Nothing ,V3 (Just 0) (Just 0 ) (Just 0) )
        Pin -> (V3 Nothing Nothing (Just 0) ,V3 (Just 0) (Just 0) (Just 0))
        Pin3D -> (V3 Nothing Nothing Nothing ,V3 (Just 0) (Just 0) (Just 0))
        SinglePin -> (V3 Nothing Nothing Nothing ,V3 Nothing Nothing (Just 0))
        Roller -> (V3 0 Nothing 0 ,V3 0 0 0 )
        Friction x -> (V3 0 Nothing 0 ,V3 0 0 0 )
    constr i  = (Just <$> 0,Just <$> 0)

momentForceEquations :: (g ~Force , Show a , Ord a ,Floating a) => Grid g a -> M.Map Int (LinkDomain g a) -> M.Map Int (NodeDomain g a) -> [a]
momentForceEquations = (\l v h -> momentForce l  v  h )


rotor :: Floating a => V3 a -> V3 a -> V3 a -> M3 a
rotor  l0@(V3 x0 y0 z0) l1 l2   = V3 tx ty  tz
  where
        tx@(V3 txx txy txz) = (1/l) *^ n
          where
            l = norm n
        ty = V3 tyx tyy tyz
          where
            tyx = tzy*txz - tzz*txy
            tyy = tzz*txx - tzx*txz
            tyz = tzx*txy - tzy*txx
        tz@(V3 tzx tzy tzz) = (1/zl) *^ V3 zx zy zz
          where
            zl = norm (V3 zx zy zz)
            zx = dz*y21 - dy*z21
            zy = dx*z21 - dz*x21
            zz = dy*x21 - dx*y21
        n@(V3 x21 y21  z21 ) = l1 ^-^ l2
        lm@(V3 xm ym zm ) = (1/2) *^(l1 ^+^ l2)
        V3 dx dy dz = l0 ^-^ lm




moment l1 l2  b = transpose (rotor (V3 0 1 0) l1 l2) !*! force b !*! rotor (V3 0 1 0 ) l1 l2

force (Link _) = 0
force (Beam l e a (V3 ix iy iz) g ) = V3 (V3 (e*a/l)  0 0) (V3 0 (12*e*iz/l^3) 0 ) (V3 0 0 (12*e*iy/l^3))
force (Bar l e a) = (V3 (V3 n 0 0) (V3 0 0 0 ) (V3 0 0 0))
    where n = e* a/l



bending (Bar _ _ _) = 0
bending (Link _ ) = 0
bending (Beam l e a (V3 ix iy iz) g)
  = V3
      (V3 0 0 0)
      (V3 0 0 (-6*e*iz/l^2))
      (V3 0 (6*e*iy/l^2) 0)

crosstorsor (Bar _ _ _ ) = 0
crosstorsor (Link  _ ) = 0
crosstorsor (Beam l e a (V3 ix iy iz) g)
  = V3
      (V3 (-g*ix/l)  0 0)
      (V3 0 (2*e*iy/l) 0 )
      (V3 0 0  (2*e*iz/l))


torsor (Bar _ _ _ ) = 0
torsor (Link  _ ) = 0
torsor (Beam l e a (V3 ix iy iz) g)
  = V3
      (V3 (g*ix/l) 0 0)
      (V3 0 (4*e*iy/l) 0 )
      (V3 0 0  (4*e*iz/l))

nodeForces lmap nvars (ix,(s,el))
  = foldr1 (\(a,b) (c,d) -> (a ^+^ c , b ^+^d)) $   (\(h,t,resh,rest,a) -> if ix == h then resh else (if ix == t then rest else error "wrong index")) . flip var lmap <$> F.toList s

linkForces g linkInPre nodesInPre
  = fmap (\(h,t,resh,rest,a) -> (norm resh , norm resh)) <$> M.toList lmap
  where
    lmap = M.fromList $ eqLink nvars <$> links g
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    nodesIn = unForces <$> nodesInPre


bendIter iter@(Iteration r i g)
  =  Iteration r i (g {shead = editNodes <$> (shead g), linksPosition = editLinks <$> linksPosition g})
    where
      lmap = M.fromList (links (grid iter))
      npmap = M.fromList (shead (grid iter))
      var2 i = fmap (fromMaybe 0) . (\(i,_,_,_) -> i). var i
      nmap = unForces. getCompose <$> M.fromList (pressures iter)
      editNodes (i,(np,nr)) =  (i, (np ^+^ d,nr))
        where
          d = var2 i nmap
      editLinks (i,l) = (i, (\(p,r) -> (p ^+^ dh , if norm (dh ^-^dt )  < 1e-3   then r else  traceShow (i,h,t,delta dh dt nhp ntp (unSO3 r) ,fmap (*(180/pi)) $unRot $ SO3 $transpose $ transpose ratio !*! unSO3 r) $  SO3 $ transpose ratio !*! unSO3 r  )) <$> l )
        where

          (h,t,le) = var i lmap
          ratio = bendingRatio  (dt ^-^ dh) (ntp ^-^ nhp)
          (nhp,_) = var h npmap
          (ntp,_) = var t npmap
          dh = var2 h nmap
          dt = var2 t nmap

delta di df  ni nf  r = ((((transpose b  !*! r ) !* (V3 (norm bl) 0 0)) ^+^ (ni ^+^ di)) ^-^ (nf ^+^ df), bl )
  where
    b = bendingRatio (df ^-^ di) (nf ^-^ ni)
    bl = (nf ^+^ df) ^-^ (ni ^+^ di)

localToGlobal v  l = rot2V3 (normalize v) (normalize l)
    where normalize l = (1/norm l) *^ l

bendingRatio d l = localToGlobal l (l ^+^ d)

volumeLink nvars npos lmap smap (ls,Tetra4 e  ) = zip p (getZipList$ getCompose $ (( kres) !* Compose (ZipList vars)))
  where kres = tetrastiffness coords   e
        sfs = (flip var smap) <$> ls
        lks = fmap (fmap (flip var lmap  )). fst <$>  sfs
        res =  fmap (\(b,(h,t,e))-> if b then (h,t) else (t,h)) <$> lks
        p = L.nub $ concat $ path <$> res
        coords =  fmap (\i->  fst $ var i npos) p
        vars =  fmap (\i-> (\(v,_,_,_) -> v )$ var i nvars ) p
volumeLink nvars npos lmap smap (ls,Hexa8 e  ) = zip p (getZipList $ getCompose $ kres !* Compose (ZipList vars))
  where kres = hexa8stiffness coords   e
        sfs = (flip var smap) <$> ls
        lks = fmap (fmap (flip var lmap  )). fst <$>  sfs
        res =  fmap (\(b,(h,t,e))-> if b then (h,t) else (t,h)) <$> lks
        p = L.nub $ concat $ path <$> res
        coords =  fmap (\i->  fst $ var i npos) p
        vars =  fmap (\i-> (\(v,_,_,_) -> v )$ var i nvars ) p


surfaceLink _  _ _ (_,FaceLoop) = []
surfaceLink nvars npos lmap (ls,Quad4 e h ) = zip p (getZipList $ getCompose $ kres !* Compose (ZipList vars))
  where
    kres = quad4stiffness coords h  e
    lks = fmap (flip var lmap  ) <$>  ls
    res =  (\(b,(h,t,e))-> if b then (h,t) else (t,h)) <$> lks
    p = reverse $ path res
    coords =  fmap (\i-> (\(V3 x y _) -> V2 x y)$ fst $ var i npos) p
    vars =  fmap (\i-> (\(V3 x y _,_,_,_) -> V2 x y)$ var i nvars ) p

eqLink nvars (i,(h,t,[el@(Link l)])) = (i,(h,t,(pure 0,pure 0) , (pure 0 ,pure 0), el))
eqLink nvars (i,(h,t,l)) =  (i,(h,t,( rtb !*  resh ,rtb !* mesh),( rtb !* rest,rtb !* mest),el))
  where
    el = justError "no beam" $ L.find isBeam l
    ((fh,mhp,_,_),(ph,_)) = nvarsEl h
    ((ft,mtp,_,_),(pt,_)) = nvarsEl t
    pd = force el
    rt = rotor (V3 0 1 0) pt ph
    rtb = transpose rt
    fhl = rt !* fh
    ftl = rt !* ft
    mh = rt !* mhp
    mt = rt !* mtp
    isBeam (Bar _ _ _) = True
    isBeam (Link  _) = True
    isBeam (Beam _ _ _ _ _) = True
    isBeam _ = False
    -- Energy Conservation
    bend = bending el
    bendt = transpose $bending el
    btor = torsor el
    ctor = crosstorsor el
    resh = (pd !* (fhl ^-^  ftl))  ^+^ (bend  !* (mh ^+^ mt))
    mesh  = ((btor !* mh ^+^ ctor !* mt) ) ^+^ (bendt !* (fhl ^-^ ftl))
    rest = (pd !* (ftl ^-^  fhl)) ^-^ (bend !* (mt ^+^ mh))
    mest  = (btor !* mt ^+^ ctor !* mh) ^+^ (bendt !* (fhl ^-^ ftl))
    nvarsEl h =  var h nvars

forces  g linkInPre nodesInPre =  Compose . fmap (\(a,b) -> Compose (V2 a b))  $nodeForces lmap nvars <$> nodesSet g
  where
    lmap = M.fromList $ eqLink nvars <$> links g
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    nodesIn = unForces <$> nodesInPre



-- momentForce :: Floating a => Grid Force a -> M.Map Int ([(V3 a,V3 a)]) -> M.Map Int (V3 a ,V3 a)  -> [a]
momentForce g linksInPre nodesInPre = concat $ nodeMerge <$> nodesSet g
  where
    nodesIn = unForces <$> nodesInPre
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    l = reverse $ links g
    nodeMerge (ix,(s,el)) = catMaybes . zipWith3 (\i f j -> if isNothing i  || isNothing f then Just j else Nothing) (F.toList a <> F.toList aa) (F.toList fv <> F.toList mv )  .zipWith (+) (F.toList m <> F.toList ma) . fmap sum .  L.transpose $ (linkEls <> sEls <> vEls)
      where (_,_,m,ma) = var ix nodesIn
            Tag a aa fv mv = tag el
            linkEls = (\(a,b) -> F.toList a <> F.toList b). (\(h,t,resh,rest,a) -> if ix == h then resh else (if ix == t then rest else error "wrong index")) . flip var lmap <$> F.toList s
            sEls = maybeToList ((<> replicate 4 0) . F.toList <$>  M.lookup ix smap)
            vEls = maybeToList ((<> replicate 3 0) . F.toList <$>  M.lookup ix cmap)
    lmap = M.fromList $ eqLink nvars <$> l
    smap = M.fromList $ concat $ surfaceLink nodesIn (M.fromList $ shead g) (M.fromList l) . snd <$>  surfaces g
    cmap = M.fromList $ concat $ volumeLink nodesIn (M.fromList $ shead g) (M.fromList l) (M.fromList (surfaces g)). snd <$>  volumes g




instance Coord Force (V3 Double) where
  nextElement  = nextS
  thisElement l i = (\(u,m,j)-> (if u /= 0 then 0 else if m /= 0 then 1 else if j/= 0 then 2 else 2,(0,SO3 . P.rotM $ (V3 (opi u) (opi m) (opi j))))) <$> thisF l i
  elemTrans t = (lengthE t , angleE t)
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

skew231 (V3 x y z) = V3 (V3 0 z (-y)) (V3 (-z) 0 x) (V3 y (-x) 0)

rot2V3  x y = identV3 !+! skewV3 v !+! ((*((1 - dot x  y )/norm v)) **^ (skewV3 v !*! skewV3 v))
  where
    v = cross x y

thisF l (Connection i _ ) = M.fromList i
thisF l e = M.fromList (fmap (fmap (0,0,)) . els . (F.toList l,) $ e)
  where
    els ([a,b,c,d,e,f],i)
      =  [(a,0),(b,0),(c,0),(d,0),(e,0),(f,0)]
    els ([a,b,c,d,e],i)
      =  [(a,0),(b,0),(c,0),(d,0),(e,0)]
    els ([a,b,c,d],i)
      =  [(a,0),(b,0),(c,0),(d,0)]
    els ([a,b,c],i)
      =  [(a,0),(b,0),(c,0)]
    els ([a,b],i)
      =  [(a,0),(b,0)]
    els ([a],i)
      =  [(a,0)]

v1,v2:: V3 Double
v1 = V3 1 2 0
v2 = V3 1 2.1 1

--- Test Link reflection

rel = [(Bar 1.0 0 0 ),BTurn (1/4,-1/4),Bar 1 0 0,BTurn (1/9,0), Bar 2 0 0] :: [Force Double]
rori = (V3 1 (-1) (1.2::Double) , SO3 $rotM (0 :: V3 Double))
