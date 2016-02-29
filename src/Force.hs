{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
module Force where
import Position hiding (rotM)
import qualified Position as P
import Domains
import Data.Monoid
import Data.Functor.Compose
import Data.Functor.Classes
import Linear.V3
import Linear.Matrix
import Linear.Vector
import Control.Arrow
import Rotation.SO3
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as F
import Debug.Trace

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
  deriving(Eq,Ord,Functor,Show)

data Force a
  = Support  (Support a)
  | Load2D a a
  | Load3D (V3 a) (V3 a)
  | Load
  | Beam a
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
  revElem (BTurn (a,b) )  = (BTurn (-a,-b))
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
        Pin3D -> (V3 Nothing Nothing Nothing ,V3 (Just 0) (Just 0) (Just 0))
        SinglePin -> (V3 Nothing Nothing Nothing ,V3 Nothing Nothing (Just 0))
        Roller -> (V3 (Just 0) Nothing (Just 0) ,V3 (Just 0) (Just 0) (Just 0) )
        Friction x -> (V3 (Just 0) Nothing (Just 0) ,V3 (Just 0) (Just 0) (Just 0) )
    constr i  = (Just <$> 0,Just <$> 0)

momentForceEquations = (\l v h -> momentForce l (fmap unForces . getCompose <$> v) (unForces <$> h) )

momentForce g linksIn nodesIn = fmap sum $  concat eqs
  where
    eqs = traceShowId $ eqLink <$> l
    forces vars = transpose $ fmap (\((f,m),(p,r)) ->  (unSO3 r !*! rotM pi)!*  f) vars
    moments vars = transpose $ fmap (\(((f,m)),(p,r)) ->  m ^+^ ((unSO3 r  !*! rotM pi ) !*  f ) `cross` p )  vars
    nvars = M.fromList $ fmap (\((ix,i),v) -> (ix,(i,v))) $ zip  (M.toList nodesIn) (reverse $ snd <$> shead g)
    l = reverse $ links g
    -- eqLink (i,h,t,l) =  ( F.toList $forces vars )<>  ( F.toList $ moments vars)
    eqLink (i,h,t,l) =  (L.take 2 $  F.toList $forces vars )<>  ( L.drop 2 $ F.toList $ moments vars)
      where
        pos = zip ((\(i,j) -> (i , j)) <$> var i linksIn ) (var i (M.fromList (linksPosition g)))
        vars = [nvarsEl False h ] <> pos <> [ nvarsEl True t ]
        nodeEq b (Support (Friction f)) ((fv@(V3 fvx fvy fvz),m),p@(_,rot)) = ((atrito ,m),p)
          where atrito
                  | x >= 0 = V3 0 fvy fvz
                  | otherwise = V3 (fvy*f) fvy fvz
                V3 _ x _ = (unSO3 rot  !*! rotM pi) !* fv

        nodeEq b _ i = i
        nvarsEl b h = nodeEq b (var h (M.fromList (nodesFlow g))) ((if b then first (first (fmap negate) . fmap (fmap negate)) else id ) $ var h nvars)



instance Coord Force (V3 Double) where
  nextElement  = nextS
  thisElement l i = (\j-> (0,SO3 $ P.rotM $ fmap opi $ (V3 0 0 j))) $ thisF l i
  elemTrans t = (lengthE t , angleE t)
    where
      angleE  = SO3 . P.rotM . (\i-> opi i ) . angE
        where
          angE (BeamTurn  r  ) = r3 (0,0,r)
          angE (BTurn  (r,c)  ) = r3 (0,r,c)
          angE  i = r3 (0,0,0)
      lengthE (Beam  c ) = r3 (c,0,0)
      lengthE i = 0
      r3 (x,y,z) = V3 x y z


thisF l e  = justError "no el" $ M.lookup l  (M.fromList ( els $ first F.toList  e))
  where
    els ([a,b,c],i)
      =  [(a,0),(b,1/2),(c,1/2)]
    els ([a,b],i)
      =  [(a,0),(b,1/2)]
    els ([a],i)
      =  [(a,0)]


