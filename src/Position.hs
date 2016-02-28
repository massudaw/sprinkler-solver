{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Position where

import Domains
import Grid
import Debug.Trace
import Control.Arrow
import qualified Data.Foldable as F
import Data.Maybe
import Data.Distributive
-- import Element
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad.Trans.State
import Linear.V3
import Linear.V1
import Linear.Matrix ((!*!),(!*))
import Rotation.SO3 hiding (rotM)

import Diagrams.Prelude

class RBackend a where
  type TCoord a
  transformElement  ::  (TCoord a,Ang (TCoord a)) -> a -> a
  errorItem :: a

class RBackend a => Target sys a  where
  renderNode :: [Double] -> (S.Set Int,(Double,(Int,sys Double))) -> a
  renderLink ::  (Double,Double) -> (Int,Int) ->  Int  -> Int -> sys Double -> a



about (ix,iy,iz) = transform (aboutX (ix @@ turn)) . transform (aboutZ (iz @@ turn))  . transform (aboutY (iy @@ turn))




subSp (i,b) (j,c) = (i ^-^ j, SO3 $  distribute (unSO3 c) !*! unSO3  b )

nextS :: Coord f (V3 Double) => Int -> (S.Set Int,(Int,f Double)) -> [(Int,(V3 Double,SO3 Double))]
nextS l v@(p,_)  = fmap (\i -> (i,subSp (0,SO3 $ rotM (V3 0 0 pi)) $ subSp  (thisElement i v) (thisElement l v))) $ filter (/=l) (F.toList p )


instance PreCoord (V3 Double) where
  type Ang (V3 Double) = SO3 Double
  dist i j = distance (r2p i) (r2p j)
  trans (l,i) (lo,a) = ( l + unSO3 i !* lo  , SO3 $ unSO3 i !*!  unSO3 a )



rot (V3 ix iy iz) = rotY (V1 iy) !*! rotZ (V1  iz)  !*! rotX (V1  ix)

rotD (V3 ix iy iz) = distribute (rotX (V1 ix)) !*! (distribute (rotZ (V1  iz))  !*! distribute (rotY (V1  iy)))

opi i = i * 2 *pi
upi i = i/(2 *pi)


locateGrid
  :: (Coord f a, Show (f Double),Show (Ang a), Show a,  Num a, Monad m) =>
     M.Map Int (Int, Int, Int, [ f Double ])
     -> M.Map Int (S.Set Int, (Int, f Double ))
     -> Int
     -> (a, Ang a)
     -> Either
          (Int, Int, Int, [f Double])
          (S.Set Int, (Int, f Double))
     -> StateT (M.Map Int (a, Ang a), M.Map Int [(a, Ang a)]) m ()
locateGrid lmap nmap l r (Right oe@(s,(e@(n,_)))) = do
  let
      t =  thisElement l (s,e)
  modify (<> (M.singleton n ( trans r t),mempty))
  let trav ne@(i,coo)  =  do
        let pos = trans r coo
        locateGrid lmap nmap n  pos (Left $ var i lmap )
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
             then return ()
             else traceShow ("link (" <> show l <> ") non exact union node (" <> show h <> ") " <> show dt <> " /= " <>  show (fst $ var h visitedNode)) $ (return ())

r2p = p3 . unr3

rotM = rotD

transElemr e =  trans (elemTrans e)
transEleml i e =  trans i (elemTrans e)



varM i j = case M.lookup i j of
              Nothing ->  Nothing
              i -> i
drawGrid iter = L.foldr1 (<>) $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter
        styleNodes  it = catMaybes $ fmap (\i -> do
                pos <- varM (fst i) gridMap
                -- pres <- varM (fst i) (M.fromList (pressures it))
                let pres = 0
                return $ transformElement  pos $ renderNode [0,0] (S.empty ,((abs $ fst pos ^. _z ) *0 + pres,i))) (nodesFlow it)
          where -- metrics = [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                gridMap = (M.fromList (shead $ it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,h,t,i)  -> do
                    pos <- varM l  posMap
                    return $ catMaybes $ zipWith3 (\m ix n ->  do
                      let flow = 0
                      return $ transformElement m $ renderLink (flow ,1 ) (h,t) ix  l n ) pos  [0..] i ) (links (it))
          where -- [max,min]= [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                -- nf f =  abs f /(max - min)
                posMap = M.fromList $ linksPosition (it)
                -- flowMap  = M.fromList (flows it)


-- styleNodes :: Iteration Double -> [Mecha.Solid]
drawIter iter = L.foldr1 (<>) $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter
        styleNodes  it = catMaybes $ fmap (\i -> do
                pos <- varM (fst i) gridMap
                -- pres <- varM (fst i) (M.fromList (pressures it))
                let pres = 0
                return $ transformElement  pos $ renderNode [0,0] (S.empty ,((abs $ fst pos ^. _z ) *0 + pres,i))) (nodesFlow (grid it))
          where -- metrics = [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                gridMap = (M.fromList (shead $ grid it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,h,t,i)  -> do
                    pos <- varM l  posMap
                    return $ catMaybes $ zipWith3 (\m ix n ->  do
                      let flow = 0
                      return $ transformElement m $ renderLink (flow ,1 ) (h,t) ix  l n ) pos  [0..] i ) (links (grid it))
          where -- [max,min]= [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                -- nf f =  abs f /(max - min)
                posMap = M.fromList $ linksPosition (grid it)
                -- flowMap  = M.fromList (flows it)

drawIterGraph  iter = L.foldr1 (<>) $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter
        styleNodes  it = catMaybes $ fmap (\i -> do
                let  pres  = Just 0 -- varM (fst i) (M.fromList (pressures it))
                return $ renderNode [0,0] (S.empty ,(fromMaybe 0  pres,i))) (nodesFlow (it))
          where
                gridMap = (M.fromList (shead $ it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,h,t,i)  -> do
                    return $ catMaybes $ zipWith3 (\m ix n ->  do
                      return $ renderLink (0 ,0 ) (h,t) ix  l n ) [0..] [0..] i ) (links (it))



revElems :: (PreSys f ,Num a) => [f a ] -> [f a]
revElems = reverse .fmap revElem

