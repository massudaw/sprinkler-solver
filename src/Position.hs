{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Position where

import Control.Monad
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
  renderNode :: S.Set Int -> Int -> sys Double -> a
  renderLink :: (Int,Int) -> Int -> Int -> sys Double -> a
  renderNodeSolve :: NodeDomain sys Double -> Int -> sys Double ->  a
  renderLinkSolve :: LinkDomain sys Double -> sys Double ->  a



about (ix,iy,iz) = transform (aboutX (ix @@ turn)) . transform (aboutZ (iz @@ turn))  . transform (aboutY (iy @@ turn))

subSp (i,b) (j,c) = (i ^-^ j, SO3 $   distribute (unSO3 c) !*! (unSO3  b )  )

nextS :: Coord f (V3 Double) => Int -> (S.Set Int,(Int,f Double)) -> [(Int,(V3 Double,SO3 Double))]
nextS l v@(p,_)  = fmap (\i -> (i,subSp (0,SO3 $ rotM (V3 0 0 pi)) $ subSp  (tElement i v) (tElement l v))) $ filter (/=l) (F.toList p )


tElement l = justError ("no el " <> show l) . M.lookup l . thisElement

instance PreCoord (V3 Double) where
  type Ang (V3 Double) = SO3 Double
  dist (i,ir) (j,jr) = (distance (r2p i) (r2p j) ,norm $ angDist ir jr)
    where angDist i j = unRot231 $ SO3 $ distribute $ (distribute $ unSO3 i) !*! ( unSO3 j)
  trans (l,i) (lo,a) = ( l + unSO3 i !* lo  , SO3 $ unSO3 i !*!  unSO3 a )
  untrans (l,i) (lo,a) = ( l ^-^  unSO3 i !* lo  , SO3 $ unSO3 i !*! distribute (unSO3 a )  )


rot (V3 ix iy iz) = rotY (V1 iy) !*! rotZ (V1  iz)  !*! rotX (V1  ix)

rotD (V3 ix iy iz) = distribute (rotX (V1 ix)) !*! (distribute (rotZ (V1  iz))  !*! distribute (rotY (V1  iy)))

opi i = i * 2 *pi
upi i = i/(2 *pi)

unrot = fmap upi. unRot231 . SO3 . distribute . unSO3 .snd

locateGrid
  :: (SO3 Double ~ (Ang a) , Coord f a, Show (f Double),Show (Ang a), Show a,  Num a, Monad m) =>
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
      t =   tElement  l (s,e)
      rnew = trans r t
  modify (<> (M.singleton n rnew,mempty))
  let trav ne@(i,coo)  =  do
        let pos = trans r coo
        (_,visitedLink) <- get
        -- when (isNothing $ M.lookup i visitedLink) $ do
        locateGrid lmap nmap n  pos (Left $ var i lmap )
        --   return ()
  mapM trav  (nextElement l (s,e))
  return ()

locateGrid lmap nmap n r ll@(Left (l,h,t,e))
  | n == h =  do
    i <- path t e
    modify (<> (mempty ,M.singleton l i))
  | n == t = do
    -- i <- revpath h e
    modify (<> (mempty ,mempty )) -- M.singleton l i))
  | otherwise = error $ "wrong back element " <> show n  <> " " <> show ll
  where
    {-revpath h e = do
        let
            (pr,rr) =  traceShow "rev" r
            sn = scanr transElemr   r e -- (pr , SO3 $ ( distribute $ unSO3 rr )) e
            (psn,rsn) = head sn
            nxt  = (psn,SO3 $ distribute $ unSO3 rsn )
        nextNode  (head sn ) h
        return (init sn)-}
    path  h e = do
        let
            sn =  scanl transEleml r  e
        nextNode  (last sn) h
        return (init sn)
    nextNode  pos@(dt ,a) h = do
        (visitedNode,_) <- get
        let dis =  dist pos  (var h visitedNode)
        if  not $ M.member h visitedNode
          then do
            locateGrid lmap nmap l pos (Right $ var h nmap)
          else
            if   dis < (1e-2,1e-2)
             then return ()
             else do
                let warnPos =  "link (" <> show l <> ") non exact union node (" <> show h <> ") " <> show dt <> " /= " <>  show (fst $ var h visitedNode) <> " distance = " <> show (fst dis)
                    warnAngle =  "link (" <> show l <> ") non exact angle node (" <> show h <> ") " <> show a <> " /= " <>  show (snd $ var h visitedNode)<> " distance = " <> show (dis)
                traceShow warnPos $ traceShow warnAngle (return ())

r2p = p3 . unr3

rotM = rotD

transElemr e =  flip untrans (elemTrans e)
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
                return $ transformElement  pos $ renderNode S.empty (fst i ) (snd i) ) (nodesFlow it)
          where -- metrics = [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                gridMap = (M.fromList (shead $ it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,h,t,i)  -> do
                    pos <- varM l  posMap
                    return $ catMaybes $ zipWith3 (\m ix n ->  do
                      let flow = 0
                      return $ transformElement m $ renderLink  (h,t) ix  l n ) pos  [0..] i ) (links (it))
          where -- [max,min]= [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                -- nf f =  abs f /(max - min)
                posMap = M.fromList $ linksPosition (it)
                -- flowMap  = M.fromList (flows it)

mergeStates i x = fst $ runState( traverse parse i) (F.toList x)

-- styleNodes :: Iteration Double -> [Mecha.Solid]
drawIter iter = L.foldr1 (<>) $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter
        styleNodes  it = catMaybes $ fmap (\i -> do
                pos <- varM (fst i) gridMap
                pres <- varM (fst i) (M.fromList (pressures it))
                let nstate = mergeStates (constrained (snd i))  pres
                return $ transformElement  pos $ (renderNode  S.empty (fst i) (snd i) <> renderNodeSolve nstate (fst i) (snd i) )) (nodesFlow (grid it))
          where -- metrics = [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                gridMap = (M.fromList (shead $ grid it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,h,t,i)  -> do
                    pos <- varM l  posMap
                    return $ catMaybes $ zipWith3 (\m ix n ->  do
                      let flow = 0
                      return $ transformElement m $ renderLink  (h,t) ix  l n ) pos  [0..] i ) (links (grid it))
          where -- [max,min]= [maximum (snd <$> flows it), minimum (snd <$> flows it)]
                -- nf f =  abs f /(max - min)
                posMap = M.fromList $ linksPosition (grid it)
                -- flowMap  = M.fromList (flows it)


drawIterGraph  iter = L.foldr1 (<>) $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter
        styleNodes  it = catMaybes $ fmap (\i -> do
                return $ renderNode S.empty  (fst i) (snd i) ) (nodesFlow (it))
          where
                gridMap = (M.fromList (shead $ it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,h,t,i)  -> do
                    return $ catMaybes $ zipWith3 (\m ix n ->  do
                      return $ renderLink  (h,t) ix  l n ) [0..] [0..] i ) (links (it))



revElems :: (PreSys f ,Num a) => [f a ] -> [f a]
revElems = reverse .fmap revElem

reflectPath el  ori = (compareS res ori , zipWith compareS fpath (bpath) ,zip fpath bpath)
  where
    res = foldr transElemr  end el
    end  = foldl transEleml  ori el
    fpath  = scanl transEleml ori el
    bpath = scanr transElemr end el
    compareS res ori = dist res ori < (1e-9 , 1e-9)
    ang res = (unRot231 . SO3  . distribute . unSO3) res


