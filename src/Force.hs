{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
module Force where
import Position
import qualified Position as P
import Data.Maybe
import Domains
import Data.Monoid
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
  | Tag  (V3 (Maybe a)) (V3 (Maybe a))
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
  | BeamTurn a
  | BTurn (a,a)
  deriving(Eq,Ord,Functor,Show)

newtype Forces a = Forces {unForces :: (V3 a, V3 a)} deriving (Foldable,Functor,Traversable,Show)


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
    constr (Load2D i a ) =(Just <$> V3 i a 0 ,Just <$> 0)
    constr (Load3D v m  ) =(Just <$> v , Just <$> m )
    constr Load = (V3 Nothing (Just 0)  (Just 0) ,Just <$> 0)
    constr i  = (Just <$> 0,Just <$> 0)
  constrained =  Forces . constr
    where
    constr (Support s ) =
      case s of
        FixedSupport2D -> (V3 Nothing Nothing (Just 0) ,V3 (Just 0) (Just 0) Nothing)
        FixedSupport3D -> (V3 Nothing Nothing Nothing ,V3 Nothing Nothing Nothing)
        SmoothSurface -> (V3 (Just 0) (Just 0) Nothing ,V3 (Just 0) (Just 0 ) (Just 0) )
        Pin -> (V3 Nothing Nothing (Just 0) ,V3 (Just 0) (Just 0) (Just 0))
        Tag t l -> (t , l )
        Pin3D -> (V3 Nothing Nothing Nothing ,V3 (Just 0) (Just 0) (Just 0))
        SinglePin -> (V3 Nothing Nothing Nothing ,V3 Nothing Nothing (Just 0))
        Roller -> (V3 (Just 0) Nothing (Just 0) ,V3 (Just 0) (Just 0) (Just 0) )
        Friction x -> (V3 (Just 0) Nothing (Just 0) ,V3 (Just 0) (Just 0) (Just 0) )
    constr i  = (Just <$> 0,Just <$> 0)

momentForceEquations :: (g ~Force , Show a , Ord a ,Floating a) => Grid g a -> M.Map Int (LinkDomain g a) -> M.Map Int (NodeDomain g a) -> [a]
momentForceEquations = (\l v h -> momentForce l  v  h )

symetric (V3 dx dy dz)  =  V3 (V3 (dx * dx) ( dx* dy) (dx*dz)) (V3 (dx*dy) (dy*dy) (dz*dy)) (V3 (dz*dx) (dy*dz) (dz*dz))
sym4 x = V2 (V2 (symetric x ) (negate **^ symetric x)) (V2 (negate **^ symetric x) (symetric x))

infixr 5 **^
s **^ c = fmap (fmap s) c

moment :: Floating a => V3 a -> a -> a -> V3 (V3 a)
moment l e a  = ((e*a/(norm l)^3)*)**^ symetric l

forces  g linkInPre nodesInPre = nodeForces lmap nvars <$> nodesSet g
  where
    lmap = M.fromList $ eqLink nvars <$> links g
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    nodesIn = unForces <$> nodesInPre


forces  g linkInPre nodesInPre = nodeForces lmap nvars <$> nodesSet g
  where
    lmap = M.fromList $ eqLink nvars <$> links g
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    nodesIn = unForces <$> nodesInPre

nodeForces lmap nvars (ix,(s,Support (Tag a  _))) = foldr1 (^+^) $   (\(h,t,resh,rest,a) -> if ix == h then resh else (if ix == t then rest else error "wrong index")) . flip var lmap <$> F.toList s

linkForces g linkInPre nodesInPre = fmap (\(h,t,resh,rest,a) -> (norm resh , norm resh/a)) <$> M.toList lmap
  where
    lmap = M.fromList $ eqLink nvars <$> links g
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    nodesIn = unForces <$> nodesInPre


bendIter iter@(Iteration r i g) =  Iteration r i (g {shead = editNodes <$> (shead g), linksPosition = editLinks <$> linksPosition g})
    where
      lmap = M.fromList (fmap (\(i,h,t,l)-> (i,(h,t,l))) $ links (grid iter))
      var2 i = fmap (negate . fromMaybe 0) . fst . var i
      nmap = unForces. getCompose <$> M.fromList (pressures iter)
      editNodes (i,(np,nr)) =  (i, (np ^+^ d,nr))
        where d = var2 i nmap
      editLinks (i,l) = (i, fmap (\(p,r) -> (p ^+^ dh , if norm (dh ^-^dt )  < 1e-2 || norm p < 1e-2  then r else SO3 $ unSO3 r !*! transpose (bendingRatio  (dh ^-^dt) p  ))) l )
        where (h,t,le) = var i lmap
              dh = var2 h nmap
              dt = var2 t nmap

bendingRatio d l = rot2V3 ((1/norm l)*^l) ((1/norm (l ^+^ d) ) *^ (l ^+^ d))

eqLink nvars (i,h,t,l) =  (i,(h,t,resh,rest,a))
      where
        Beam _ m a = justError "no beam" $ L.find isBeam l
        ((fh,_),(ph,_)) = nvarsEl False h
        ((ft,_),(pt,_)) = nvarsEl True t
        pd = moment (pt ^-^ ph) m a
        isBeam (Beam _ _ _) = True
        isBeam _ = False
        -- Energy Conservation
        resh = pd !* (ft ^-^  fh)
        rest = pd !* (fh ^-^  ft)
        nvarsEl b h =  var h nvars


-- momentForce :: Floating a => Grid Force a -> M.Map Int ([(V3 a,V3 a)]) -> M.Map Int (V3 a ,V3 a)  -> [a]
momentForce g linksInPre nodesInPre = concat $ nodeMerge <$> nodesSet g
  where
    -- linksIn = fmap unForces . getCompose <$> linksInPre
    nodesIn = unForces <$> nodesInPre
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip (M.toList nodesIn) (snd <$> shead g)
    l = reverse $ links g
    nodeMerge (ix,(s,Support (Tag a fv ))) = catMaybes . zipWith3 (\i f j -> if isNothing i  || isNothing f then Just j else Nothing) (F.toList a) (F.toList fv)  .zipWith (+) (F.toList m) . fmap sum . L.transpose $ F.toList . (\(h,t,resh,rest,a) -> if ix == h then resh else (if ix == t then rest else error "wrong index")) . flip var lmap <$> F.toList s
      where (r,m) = var ix nodesIn
    lmap = M.fromList $ eqLink nvars <$> l



nextF :: Coord f (V3 Double) => Int -> (S.Set Int,(Int,f Double)) -> [(Int,(V3 Double,SO3 Double))]
nextF l v@(p,_)  = fmap (\i -> (i,(0,SO3 $ rotM 0))) $ filter (/=l) (F.toList p )

instance Coord Force (V3 Double) where
  nextElement  = nextS
  thisElement  i = (\j-> (0,SO3 $ P.rotM $ fmap opi $ (V3 0 0 j))) <$> thisF  i
  elemTrans t = (lengthE t , angleE t)
    where
      angleE  = SO3 . P.rotM . (\i-> opi i ) . angE
        where
          angE (BeamTurn  r  ) = r3 (0,0,r)
          angE (BTurn  (r,c)  ) = r3 (0,r,c)
          angE  i = r3 (0,0,0)
      lengthE (Beam  c  m s ) = r3 (c,0,0)
      lengthE i = 0
      r3 (x,y,z) = V3 x y z

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
