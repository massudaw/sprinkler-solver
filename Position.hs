{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Position where

import Grid
import Debug.Trace
import Lint
import GHC.Stack
import Data.Maybe
import Data.Distributive
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
import Linear.V3
import Linear.V1
import Linear.Matrix ((!*!),(!*))
import Rotation.SO3
import Linear.Affine

import qualified Language.Mecha.Types as Mecha
import qualified Language.Mecha.Solid as Mecha


import Diagrams.Prelude

-- import Diagrams.Prelude.ThreeD
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text (Text)

import Control.Lens hiding(transform)

import Data.Void

class  Target a  where
  renderNode :: [Double] -> (S.Set Int,(Double,(Int,Element Double))) -> a
  renderLink ::  (Double,Double) ->  Int  -> Int -> Element Double -> a
  errorItem :: a
  type TCoord a
  transformElement  :: Coord (TCoord a) => (TCoord a,Ang (TCoord a)) -> a -> a


class Coord a where
  type Ang a
  nextElement :: Int -> (S.Set Int,(Int,Element Double)) -> [(Int,(a,Ang a))]
  thisElement :: Int -> (S.Set Int,(Int,Element Double)) -> (a,Ang a)
  elemTrans :: Element Double -> (a,Ang a)
  trans :: (a,Ang a) -> (a,Ang a) -> (a,Ang a)
  dist :: a -> a -> Double

about (ix,iy,iz) = transform (aboutX (ix @@ turn)) . transform (aboutZ (iz @@ turn))  . transform (aboutY (iy @@ turn))

instance Coord (V3  Double) where
  type Ang (V3 Double) = (Double,Double,Double) -- Transform V3 Double
  nextElement _ (p,(n,Open i))  =  []
  nextElement _ (p,(n,Reservatorio _ _ i))  =  []
  nextElement l (s,(n,Sprinkler i _ _ _ ))  =  [(h,(0, (0,0,0)))]
    where h = case S.toList $ S.delete l  s of
            [h] -> h
            i -> error $ " empty link set " <> show s
  nextElement l (p,(n,Tee (TeeConfig [rl,b,rr] _ _ _ _) _ ) )
    | rl == l = [(rr,(0, (0,0,0))), (b,(0, (0,0,-1/4))) ]
    | rr == l = [(rl,(0, (0,0,0))), (b,(0, (0,0,1/4)))]
    | b == l = [(rr,(0, (0,0,-1/4))) , (rl,(0, (0,0,1/4) ))]
  nextElement l p = errorWithStackTrace  (show (l,p))
  thisElement l (p,(n,Open i))  =  (0,(0,0,0))
  thisElement l (p,(n,Reservatorio _ _ i))  =  (0,(0,0,0))
  thisElement l (p,(n,Sprinkler _ _ _ _ ))  =  (0,(0,0,0))
  thisElement l (p,(n,Tee (TeeConfig [rl,b,rr] _ _ _ _) _ ))
    | rl == l =  (0,(0,0,1/4))
    | rr == l =   (0,(0,0,-1/4))
    | b == l =   (0,(0,0,0))
  trans (l,i@(ix,iy,iz)) (lo,a@(ax,ay,az)) = ( l + rot i !* lo  , unr3 $ r3 i <>   r3 a )
  elemTrans t = (lengthE t , angleE t)
  dist i j = distance (r2p i) (r2p j)

rot (ix,iy,iz) = ( distribute (rotX (V1 (opi  ix))) !*! (distribute (rotZ (V1 $ opi iz))  !*! distribute (rotY (V1 $ opi iy))) )

instance Semigroup (V3 Double) where
  i <> x = i + x
opi i = i * 2 *pi
upi i = i/(2 *pi)

-- unr3 (V3 i j k) = (i,j,k)

instance Num (Double,Double,Double) where
  (a,b,c) + (i,j,k) = (a+i,b+j,c+k)
  (a,b,c) * (i,j,k) = (a*i,b*j,c*k)
  abs (a,b,c) = (abs a ,abs b ,abs c)
  fromInteger i = (fromInteger i,fromInteger i,fromInteger i)
  negate (a,b,c) = (negate a,negate b, negate c)
  signum (a,b,c) = (signum a,signum b ,signum c)


locateGrid
  :: (Coord a, Show (Ang a), Show a,  Num a, Monad m) =>
     M.Map Int (Int, Int, Int, [Element Double])
     -> M.Map Int (S.Set Int, (Int, Element Double))
     -> Int
     -> (a, Ang a)
     -> Either
          (Int, Int, Int, [Element Double])
          (S.Set Int, (Int, Element Double))
     -> StateT (M.Map Int (a, Ang a), M.Map Int [(a, Ang a)]) m ()
locateGrid lmap nmap l r (Right oe@(s,(e@(n,_)))) = do
  let
      t = thisElement l (s,e)
  modify (<> (M.singleton n (trans r t {-(fst r + fst t,snd r + snd t)-}),mempty))
  let trav (i,(ri,ai))  =  do
        let pos = trans r (ri,ai) -- (fst r + ri ,snd r  + ai)
        locateGrid lmap nmap n pos (Left $ var i lmap )
  mapM trav  (nextElement l (s,e))
  return ()

locateGrid lmap nmap n r ll@(Left (l,h,t,e))
  | n == h =  do
    i <- path t e
    modify (<> (mempty ,M.singleton l i))
  | n == t = do
    i <- path  h (revElems e)
    modify (<> (mempty ,M.singleton l $ reverse i))
  | otherwise = error $ "wrong back element " <> show n  <> " " <> show ll
  where
    path  h e = do
        let
            sn = scanl transEleml r e
        nextNode  (last sn) h
        return (init sn)
    nextNode  pos@(dt ,a) h = do
        (visitedNode,_) <- get
        if  not $ M.member h visitedNode
          then do
            locateGrid lmap nmap l pos (Right $ var h nmap)
          else
            if  dist dt ( fst $ var h visitedNode) < 1e-2
             -- then traceShow ("link (" <> show l <> ") exact union node (" <> show h <> ") " <> show dt <> " == " <>  show (fst $ var h visitedNode ))  $return ()
             then return ()
             else traceShow ("link (" <> show l <> ") non exact union node (" <> show h <> ") " <> show dt <> " /= " <>  show (fst $ var h visitedNode)) $ (return ())

r2p = p3 . unr3

angleE :: Fractional a => Element a -> (a,a,a)
angleE (Joelho _ _ (c,r) _ ) = (0,c,r)
angleE (Turn c) = (c,0,0)
angleE  i = (0,0,0)


lengthE :: Element Double -> V3 Double
lengthE (Tubo _ c _ ) = r3 (c,0,0)
lengthE i = 0

transElemr e =  trans (elemTrans e)
transEleml i e =  trans i (elemTrans e)


revElems :: Num a => [Element a ] -> [Element a]
revElems = reverse .fmap revElem
  where
    revElem (Joelho i j (a,b) k) = Joelho i j (-a,-b) k
    revElem (Turn i ) = Turn (-i)
    revElem i = i


varM i j = case M.lookup i j of
              Nothing ->  Nothing
              i -> i

-- styleNodes :: Iteration Double -> [Mecha.Solid]
styleNodes  it = catMaybes $ fmap (\i -> do
        pos <- varM (fst i) gridMap
        pres <- varM (fst i) (M.fromList (nodeHeads it))
        return $ transformElement  pos $ renderNode metrics (S.empty ,((abs $ fst pos ^. _z ) *0 + pres,i))) (nodesFlow (grid it))
  where metrics = [maximum (snd <$> flows it), minimum (snd <$> flows it)]
        gridMap = (M.fromList (shead $ grid it))
        headMap = (M.fromList (nodeHeads$ it))

--styleLinks :: Iteration Double -> [Mecha.Solid]
styleLinks it = concat $ catMaybes $  fmap (\(l,_,_,i)  -> do
            pos <- varM l  posMap
            return $ catMaybes $ zipWith3 (\m ix n ->  do
              flow <- varM l flowMap
              return $ transformElement m $ renderLink (flow ,nf flow ) ix  l n ) pos  [0..] i ) (links (grid it))
  where [max,min]= [maximum (snd <$> flows it), minimum (snd <$> flows it)]
        nf f =  abs f /(max - min)
        posMap = M.fromList $ linksPosition (grid it)
        flowMap  = M.fromList (flows it)

drawIter iter = L.foldl1' (<>) $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter


