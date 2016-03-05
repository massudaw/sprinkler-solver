{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
module Force where
import Position
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

data Force a
  = Support  (Support a)
  | Load2D a a
  | Load3D (V3 a) (V3 a)
  | Load
  | Beam { length :: a, material  :: a  , section :: a }
  | PlanBeam{ length :: a, material  :: a  , section :: a ,  inertiaMolus :: V3 a , tmodulus  :: a  }
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
    constr (Load2D i a ) =(Just <$> V3 i a 0 ,0 ,Just <$> 0,0)
    constr (Load3D v m  ) =(Just <$> v ,Just <$> 0, Just <$> m ,Just <$> 0 )
    constr Load = (V3 Nothing (Just 0)  (Just 0) ,0,Just <$> 0,0)
    constr i  = (Just <$> 0,0,Just <$> 0,0)
  constrained (Support (Tag t a l m) ) = Forces (t,a,l,m)
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
{-rotor  _  l1 l2 = (/norm l) **^ (V3 (V3 x21 y21 0 ) (V3 (-y21) x21 0) (V3 0 0 0))
  where  l@(V3 x21 y21 z21)= (l2 ^-^ l1)-}
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



infixr 5 **^
s **^ c = fmap (fmap s) c

moment l1 l2  b = transpose (rotor (V3 0 1 0) l1 l2) !*! force b !*! rotor (V3 0 1 0 ) l1 l2

force (PlanBeam l e a (V3 ix iy iz) g ) = V3 (V3 (e*a/l)  0 0) (V3 0 (12*e*iz/l^3) 0 ) (V3 0 0 (12*e*iy/l^3))
force (Beam l e a) = (V3 (V3 n 0 0) (V3 0 0 0 ) (V3 0 0 0))
    where n = e* a/l

bendingt (Beam _ _ _) = 0
bendingt (PlanBeam l e a (V3 ix iy iz) g)
  = V3
      (V3 0 0 0 )
      (V3 0 0 (6*e*iy/l^2)  )
      (V3 0 (-6*e*iz/l^2) 0 )


bending (Beam _ _ _) = 0
bending (PlanBeam l e a (V3 ix iy iz) g)
  = V3
      (V3 0 0 0)
      (V3 0 0 (-6*e*iz/l^2))
      (V3 0 (6*e*iy/l^2) 0)

crosstorsor (Beam _ _ _ ) = 0
crosstorsor (PlanBeam l e a (V3 ix iy iz) g)
  = V3
      (V3 (-g*ix/l)  0 0)
      (V3 0 (2*e*iy/l) 0 )
      (V3 0 0  (2*e*iz/l))


torsor (Beam _ _ _ ) = 0
torsor (PlanBeam l e a (V3 ix iy iz) g)
  = V3
      (V3 (g*ix/l) 0 0)
      (V3 0 (4*e*iy/l) 0 )
      (V3 0 0  (4*e*iz/l))

nodeForces lmap nvars (ix,(s,Support (Tag _ _ _  _)))
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
      lmap = M.fromList (fmap (\(i,h,t,l)-> (i,(h,t,l))) $ links (grid iter))
      npmap = M.fromList (shead (grid iter))
      var2 i = fmap (negate . fromMaybe 0) . (\(i,_,_,_) -> i). var i
      nmap = unForces. getCompose <$> M.fromList (pressures iter)
      editNodes (i,(np,nr)) =  (i, (np ^+^ d,nr))
        where
          d = var2 i nmap
      editLinks (i,l) = (i, (\(p,r) -> (p ^+^ dh , if norm (dh ^-^dt )  < 1e-2   then r else SO3 $ unSO3 r !*! transpose ratio )) <$> l )
        where
          (h,t,le) = var i lmap
          ratio = bendingRatio  (dh ^-^dt) (nhp ^-^ ntp)
          (nhp,nha) = var h npmap
          (ntp,nta) = var t npmap
          dh = var2 h nmap
          dt = var2 t nmap


localToGlobal v  l = rot2V3 (normalize v) (normalize l)
    where normalize l = (1/norm l) *^ l

bendingRatio d l = localToGlobal l (l ^+^ d)

eqLink nvars (i,h,t,l) =  (i,(h,t,( rtb !*  resh ,mesh),( rtb !* rest,mest),el))
      where
        el = justError "no beam" $ L.find isBeam l
        ((fh,mh,_,_),(ph,_)) = nvarsEl h
        ((ft,mt,_,_),(pt,_)) = nvarsEl t
        pd = force el
        rt = rotor (V3 0 1 0) pt ph
        rtb = transpose rt
        fhl = rt !* fh
        ftl = rt !* ft
        isBeam (Beam _ _ _) = True
        isBeam (PlanBeam _ _ _ _ _) = True
        isBeam _ = False
        -- Energy Conservation
        bend = bending el
        bendt = bendingt el
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
    -- linksIn = fmap unForces . getCompose <$> linksInPre
    nodesIn = unForces <$> nodesInPre
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    l = reverse $ links g
    nodeMerge (ix,(s,Support (Tag a aa fv mv ))) = catMaybes . zipWith3 (\i f j -> if isNothing i  || isNothing f then Just j else Nothing) (F.toList a <> F.toList aa) (F.toList fv <> F.toList mv )  .zipWith (+) (F.toList m <> F.toList ma) . fmap sum .  L.transpose $ (\(a,b) -> F.toList a <> F.toList b). (\(h,t,resh,rest,a) -> if ix == h then resh else (if ix == t then rest else error "wrong index")) . flip var lmap <$> F.toList s
      where (_,_,m,ma) = var ix nodesIn
    lmap = M.fromList $ eqLink nvars <$> l



nextF :: Coord f (V3 Double) => Int -> (S.Set Int,(Int,f Double)) -> [(Int,(V3 Double,SO3 Double))]
nextF l v@(p,_)  = fmap (\i -> (i,(0,SO3 $ rotM 0))) $ filter (/=l) (F.toList p )

instance Coord Force (V3 Double) where
  nextElement  = nextS
  thisElement  i = (\j-> (0,SO3 . P.rotM $ (V3 0 0 (opi j)))) <$> thisF  i
  elemTrans t = (lengthE t , angleE t)
    where
      angleE  = SO3 . P.rotM . opi . angE
        where
          angE (BeamTurn  r  ) = r3 (0,0,r)
          angE (BTurn  (r,c)  ) = r3 (0,r,c)
          angE  i = r3 (0,0,0)
      lengthE (Beam  c  m s ) = r3 (c,0,0)
      lengthE (PlanBeam  c  m s _ _ ) = r3 (c,0,0)
      lengthE i = 0
      r3 (x,y,z) = V3 x y z

skew231 (V3 x y z) = V3 (V3 0 z (-y)) (V3 (-z) 0 x) (V3 y (-x) 0)

rot2V3  x y = identV3 !+! skewV3 v !+! ((*((1 - dot x  y )/norm v)) **^ (skewV3 v !*! skewV3 v))
  where
    v = cross x y

thisF  e  =   (M.fromList ( els $ first F.toList  e))
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

rel = [(Beam 1.0 0 0 ),BTurn (1/4,-1/4),Beam 1 0 0,BTurn (1/9,0), Beam 2 0 0] :: [Force Double]
rori = (V3 1 (-1) (1.2::Double) , SO3 $rotM (0 :: V3 Double))
