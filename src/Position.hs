{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Position where

import Control.Monad
import Data.Functor.Constant
import Control.Applicative.Lift
import Domains
import Grid
import Debug.Trace
import Control.Arrow
import qualified Data.Foldable as F
import Data.Maybe
import Data.Distributive
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
  statements :: [a ] -> a

class RBackend a => Target sys a  where
  renderNode :: S.Set Int -> Int -> sys Double -> a
  renderLink :: (Int,Int) -> Int -> Int -> sys Double -> a
  renderSurface :: [(Bool,(Int,Int, [sys Double]))] -> [(Int, (V3 Double,SO3 Double))] -> sys Double -> a
  renderVolume :: [[(Bool,(Int,Int, [sys Double]))]] -> [(Int, (V3 Double,SO3 Double))] -> sys Double -> a
  renderNodeSolve :: NodeDomain sys Double -> Int -> sys Double ->  a
  renderLinkSolve :: LinkDomain sys Double -> sys Double ->  a


showErr (Other (Constant i)) = Left i
showErr (Pure i)  = Right i

about (ix,iy,iz) = transform (aboutX (ix @@ turn)) . transform (aboutZ (iz @@ turn))  . transform (aboutY (iy @@ turn))

subSp (i,b) (j,c) = (i ^-^ j, SO3 $   distribute (unSO3 c) !*! (unSO3  b )  )

nextS :: (Show (f Double),Coord f (V3 Double)) => Int -> (S.Set Int,f Double) -> [(Int,(V3 Double,SO3 Double))]
nextS l v@(p,_)  = fmap (\i -> (i,  nElement i v )) $ filter (/=l) (F.toList p )

nextE :: (Show (f Double),Coord f (V3 Double)) => Int -> (S.Set Int,f Double) -> [(Int,(V3 Double,SO3 Double))]
nextE l v@(p,_)  = fmap (\i -> (i,subSp (0,SO3 $ rotM (V3 0 0 pi)) $ subSp  (tElement i v) (tElement l v))) $ filter (/=l) (F.toList p )


-- tElement :: Show (f Double) => Int -> f Double -> TCoord (f Double)
tElementInfer l (i,e) = fmap snd . M.lookup l . thisElement i $ e
tElement l (i,e) = snd . justError (" no element " <> show l <> " in " <> show e  ) . M.lookup l . thisElement i $ e
nElement l (i,e) = case  justError (" no element " <> show (l,e) ) . M.lookup l . thisElement i $ e of
            (0,v) -> subSp (0,SO3$ rotM (V3 pi 0 0)) v
            (1,v) -> subSp (0,SO3$ rotM (V3 0 pi 0)) v
            (2,v) -> subSp (0,SO3$ rotM (V3 0 0 pi )) v

angDist i j = unRot $ SO3 $ distribute $ (distribute $ unSO3 i) !*! ( unSO3 j)
instance PreCoord (V3 Double) where
  type Ang (V3 Double) = SO3 Double
  dist (i,ir) (j,jr) = (distance (r2p i) (r2p j) ,norm $ angDist ir jr)
  trans (l,i) (lo,a) = ( l + unSO3 i !* lo  , SO3 $ unSO3 i !*!  unSO3 a )
  untrans (l,i) (lo,a) = ( l ^-^  unSO3 i !* lo  , SO3 $ unSO3 i !*! distribute (unSO3 a )  )


rot (V3 ix iy iz) = rotZ (V1 iz) !*! rotY (V1  iy)  !*! rotX (V1  ix)

rotD (V3 ix iy iz) = distribute (rotZ (V1 iz)) !*! (distribute (rotY (V1  iy))  !*! distribute (rotX (V1  ix)))
unRot = unRot123

opi i = i * 2 *pi
upi i = i/(2 *pi)

unrot = unRot . SO3 . distribute . unSO3

locateGrid
  :: (SO3 Double ~ (Ang a) , a ~ V3 Double, Coord f a, Show (f Double),Show (Ang a), Show a,  Num a, Monad m) =>
     M.Map Int (Int, Int, [ f Double ])
     -> M.Map Int (S.Set Int, f Double )
     -> Int
     -> (a, Ang a)
     -> Int
     -> Either
          (Int, Int, [f Double])
          (S.Set Int, f Double)
     -> StateT (M.Map Int (a, Ang a), M.Map Int [(a, Ang a)]) m (Errors [(Int,Int,String,Double)] [(Int,Int,(a,Ang a))])
locateGrid lmap nmap l r n (Right oe@(s,e)) = do
  let
      t = tElement  l (s,e)
      rnew = trans r t
  modify (<> (M.singleton n rnew,mempty))
  let trav ne@(i,coo)  =  do
        let pos = trans rnew coo
        (_,visitedLink) <- get
        locateGrid lmap nmap n  pos i (Left $ var i lmap )
  l <- mapM trav  (nextElement l (s,e))
  return (foldl (liftA2 mappend) (pure []) l )

locateGrid lmap nmap n r l ll@(Left (hn,tn,e))
  | n == hn =  do
    (i,err) <- path tn e
    modify (<> (mempty ,M.singleton l i))
    return err
  | n == tn = do
    {-(i,err) <- revpath hn e
    modify (<> (mempty ,M.singleton l i))
    return err-}
    return (pure [])
  | otherwise = error $ "wrong back element " <> show n  <> " " <> show ll
  where
    {-revpath  nn e = do
      let
        sn =  scanr transElemr r  e
      err <- nextNode  (head sn) nn
      return (init sn,err)-}
    path  nn e = do
      let
        sn =  scanl transEleml r  e
      err <- nextNode  (last sn) nn
      return (init sn,err)
    nextNode  pos@(dt ,a) nn  = do
        (visitedNode,_) <- get
        if  not $ M.member nn  visitedNode
          then do
            locateGrid lmap nmap l pos nn  (Right $ var nn nmap)
          else do
            let
              npos = var nn visitedNode
            case tElementInfer l  (var nn  nmap)of
              Just el -> do
                let
                  pos2 = trans pos el
                  (pdis,adis) = dist pos2  npos
                p <- if pdis < 1e-2
                 then return (pure [] )
                 else return (failure [(l ,nn,show (fst pos2 )<> " /= " <>  show (fst npos) , pdis)])
                a <- if  adis < 1e-2
                 then return (pure [])
                 else return (failure [(l, nn,  show (unrot (snd pos),unrot (snd pos2)) <> " /= " <>  show (unrot $ snd npos) <> "  " <> show (angDist (snd pos2) (snd npos)), adis)])
                return $ mappend <$> p <*>  a
              Nothing ->  return $ pure [(l,nn,(V3 0 0 0,SO3 $ (unSO3 $ snd npos ) !*! distribute (unSO3 $ snd pos )))]



r2p = p3 . unr3

rotM = rotD

transElemr e =  flip untrans (elemTrans e)
transEleml i e =  trans i (elemTrans e)



varM i j = case M.lookup i j of
              Nothing ->  Nothing
              i -> i

drawGrid iter = statements  $ styleNodes iter <> styleLinks iter <> {- styleSurfaces iter <> -}styleVolume iter
  where
    styleNodes  it = catMaybes $ fmap (\i -> do
            pos <- varM (fst i) gridMap
            let pres = 0
            return $ transformElement  pos $ renderNode S.empty (fst i ) (snd i) ) (nodesFlow it)
      where
        gridMap = (M.fromList (shead $ it))

    styleLinks it = concat $ catMaybes $  fmap (\(l,(h,t,i))  -> do
                pos <- varM l  posMap
                return $ catMaybes $ zipWith3 (\m ix n ->  do
                  return $ transformElement m $ renderLink  (h,t) ix  l n ) pos  [0..] i ) (links (it))
      where
        posMap = M.fromList $ linksPosition (it)

styleSurfaces it = catMaybes $  fmap (\(n,(h,i))  -> do
                let paths = fmap (fmap (\l -> var l lEls)) h
                    nodes = fmap (\(h,t,_)->  [(h,var h npos),(t,var t npos)]) (snd <$>paths)
                return $ renderSurface paths (concat nodes)  i ) (surfaces it)
      where
        lEls =  M.fromList $ links it
        npos = M.fromList $ shead it

styleVolume it = catMaybes $  fmap (\(n,(h,i))  -> do
                let surfs = fmap (\i -> fst $ var i lSurfs) h
                    paths = fmap (fmap (\l -> var l lEls)) <$> surfs
                    nodes = fmap (\(h,t,_)->  [(h,var h npos),(t,var t npos)]) $ (snd <$> concat paths)
                return $ renderVolume paths (concat nodes)  i ) (volumes it)
      where
        lEls =  M.fromList $ links it
        lSurfs =  M.fromList $ surfaces it
        npos = M.fromList $ shead it



mergeStates i x = fst $ runState( traverse parse i) (F.toList x)

drawIter iter = statements $ nds <> lds <> styleSurfaces (grid iter) <> styleVolume (grid iter)
  where
    nds = styleNodes iter
    lds = styleLinks iter
    styleNodes  it = catMaybes $ fmap (\i -> do
            pos <- varM (fst i) gridMap
            pres <- varM (fst i) (M.fromList (pressures it))
            let nstate = mergeStates (constrained (snd i))  pres
            return $ transformElement  pos $ (renderNode  S.empty (fst i) (snd i) <> renderNodeSolve nstate (fst i) (snd i) )) (nodesFlow (grid it))
      where
        gridMap = (M.fromList (shead $ grid it))

    styleLinks it = concat $ catMaybes $  fmap (\(l,(h,t,i))  -> do
                pos <- varM l  posMap
                return $ catMaybes $ zipWith3 (\m ix n ->  do
                  let flow = 0
                  return $ transformElement m $ renderLink  (h,t) ix  l n ) pos  [0..] i ) (links (grid it))
      where
        posMap = M.fromList $ linksPosition (grid it)


drawIterGraph  iter = statements $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter
        styleNodes  it = catMaybes $ fmap (\i -> do
                return $ renderNode S.empty  (fst i) (snd i) ) (nodesFlow (it))
          where
                gridMap = (M.fromList (shead $ it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,(h,t,i))  -> do
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
    ang res = (unRot . SO3  . distribute . unSO3) res


