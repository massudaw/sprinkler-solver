{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Position where

import Control.Applicative
import Control.Applicative.Lift
import Data.Functor.Classes
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.State
import Data.Distributive
import qualified Data.Foldable as F
import Data.Functor.Constant
import Data.Map (Map)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Domains
import Grid
import Linear.Matrix
import Linear.Metric
import Linear.V1
import Linear.V3
import Linear.Vector
import qualified MultiLinear.Class as Multi
import Rotation.SO3 hiding (rotM)

showErr :: Lift (Constant a) b -> Either a b
showErr (Other (Constant i)) = Left i
showErr (Pure i) = Right i

subSp :: (Multi.MultiLinear (Ang f), Additive f, Num a1, Num a2) => (f a1, Ang f a2) -> (f a1, Ang f a2) -> (f a1, Ang f a2)
subSp (i, b) (j, c) = (i ^-^ j, Multi.transpose c Multi.!*! b)

nextS :: (Additive t , RealFloat b, Coord f t ) => Int -> [Int] -> f b -> [(Int, (t b, Ang t b))]
nextS l p v = fmap (\i -> (i, nElement i p v)) $ filter (/= l) p

nextE :: (Show b, Additive t , Multi.MultiLinear (Ang t),RealFloat b, Show1 f , Coord f t ) => Int -> [Int] -> f b -> [(Int, (t b, Ang t b))]
nextE l p v = fmap (\i -> (i, subSp (nElement i p v) ini)) $ filter (/= l) p
  where
    ini = tElement l p v

-- tElement :: Show (f Double) => Int -> f Double -> TCoord (f Double)
tElementInfer :: (Coord sys a, RealFloat b) => Int -> [Int] -> sys b -> Maybe (a b, Ang a b)
tElementInfer l i e = fmap snd . M.lookup l . thisElement i $ e

tElement :: (Show b, Show1 sys, Coord sys a, RealFloat b) => Int -> [Int] -> sys b -> (a b, Ang a b)
tElement l i e = snd . justError (" no element link " <> show l <> " in " <> show1 e) . M.lookup l . thisElement i $ e

angDist :: RealFloat a => SO3 a -> SO3 a -> V3 a
angDist i j = unRot $ SO3 $ distribute $ (distribute $ unSO3 i) !*! (unSO3 j)


nElement :: forall a b sys . (Additive a , Coord sys a , RealFloat b)  => Int -> [Int] -> sys b -> (a b, Ang a b)
nElement l i e = case justError " no element node"  . M.lookup l . thisElement i $ e of
    (dim , v) -> untrans (zero , rot @a @_ dim pi :: Ang a b ) v


instance PreCoord V3 where
  type Ang V3 = SO3
  dist (i, ir) (j, jr) = (distance (i) (j), norm $ angDist ir jr)
  trans (l, i) (lo, a) = (l + unSO3 i !* lo, SO3 $ unSO3 i !*! unSO3 a)
  untrans (l, i) (lo, a) = (l ^-^ unSO3 i !* lo, SO3 $ unSO3 i !*! distribute (unSO3 a))
  rot l v = case l of
    0 -> so3 (V3 v 0 0)
    1 -> so3 (V3 0 v 0)
    2 -> so3 (V3 0 0 v)


so3 :: Floating a => V3 a -> SO3 a
so3 = SO3 . rotM

rot3 :: Floating b => V3 b -> V3 (V3 b)
rot3 (V3 ix iy iz) = rotZ (V1 iz) !*! rotY (V1 iy) !*! rotX (V1 ix)

rotD :: Floating a => V3 a -> V3 (V3 a)
rotD (V3 ix iy iz) = distribute (rotZ (V1 iz)) !*! (distribute (rotY (V1 iy)) !*! distribute (rotX (V1 ix)))

rot132 :: Floating a => V3 a -> V3 (V3 a)
rot132 (V3 ix iy iz) = (distribute (rotY (V1 iy)) !*! distribute (rotZ (V1 iz)) !*! distribute (rotX (V1 ix)))

unRot :: RealFloat a => SO3 a -> V3 a
unRot = unRot123

opi :: Floating a => a -> a
opi i = i * 2 * pi

upi :: Floating a => a -> a
upi i = i / (2 * pi)

unrot :: RealFloat a => SO3 a -> V3 a
unrot = unRot . SO3 . distribute . unSO3

locateGrid ::
  (Show b, Additive a , Multi.MultiLinear (Ang a), RealFloat b, Coord f a, Show1 f , Show1 (Ang a) , Show1 a ,  Monad m) =>
  M.Map Int (Int, Int, [f b]) ->
  M.Map Int ([Int], f b) ->
  Int ->
  (a b, Ang a b) ->
  Int ->
  Either
    (Int, Int, [f b])
    ([Int], f b) ->
  StateT (M.Map Int (a b, Ang a b), M.Map Int [(a b, Ang a b)]) m (Errors [(Int, Int, String, b)] [(Int, Int, (a b, Ang a b))])
locateGrid lmap nmap l r n (Right oe@(s, e)) = do
  let t = tElement l s e
      rnew = trans r t
  modify (<> (M.singleton n rnew, mempty))
  let trav ne@(i, coo) = do
        let pos = trans rnew coo
        locateGrid lmap nmap n pos i (Left $ var i lmap)
  l <- mapM trav (nextS l s e)
  return (foldl (liftA2 mappend) (pure []) l)
locateGrid lmap nmap n r l ll@(Left (hn, tn, e))
  -- render Link Forward
  | n == hn = do
    (i, err) <- do
      let es = var tn . M.fromList . nextE hn [hn, tn] <$> e
          sn = scanl trans r es
      err <- nextNode (last sn) tn
      return (init sn, err)
    modify (<> (mempty, M.singleton l i))
    return err
  -- render Link Reverse
  | n == tn = do
    (i, err) <- do
      let es = var hn . M.fromList . nextE tn [hn, tn] <$> e
          sn = scanr (flip untrans) r es
      err <- nextNode (head sn) hn
      return (tail sn, err)
    modify (<> (mempty, M.singleton l i))
    return err
  | otherwise = return $ failure [(l, n, "cant find element " <> show n <> " " <> show hn <> show tn <> L.intercalate "," (show1 <$> e), 0)]
  where
    nextNode pos@(dt, a) nn = do
      (visitedNode, _) <- get
      if not $ M.member nn visitedNode
        then do
          locateGrid lmap nmap l pos nn (Right $ var nn nmap)
        else do
          let npos = var nn visitedNode
          case uncurry (tElementInfer l) (var nn nmap) of
            Just el -> do
              let pos2 = trans pos el
                  (pdis, adis) = dist pos2 npos
              p <-
                if pdis < 1e-2
                  then return (pure [])
                  else return (failure [(l, nn, show1 (fst pos2) <> " /= " <> show1 (fst npos), pdis)])
              a <-
                if adis < 1e-2
                  then return (pure [])
                  else return (failure [(l, nn,  (show1 $ Multi.transpose (snd pos)) <> "," <>   show1 (Multi.transpose (snd pos2)) <> " /= " <> show1 (Multi.transpose $ snd npos) , adis)])
              return $ mappend <$> p <*> a
            Nothing -> return $ pure [(l, nn, (zero, (snd npos) Multi.!*! Multi.transpose (snd pos)))]

rotM :: Floating a => V3 a -> V3 (V3 a)
rotM = rotD

drawGrid ::
  ( TField a ~ Double,
    Show (sys Double),
    Target sys a,
    TCoord a ~ V3,
    Ang (TCoord a) ~ SO3
    -- , a ~ Double
  ) =>
  CoordinateGrid V3 Double -> Grid sys Double ->
  a
drawGrid coord iter = statements $ styleNodes <> styleLinks <> {- styleSurfaces iter <> -} styleVolume coord iter
  where
    styleNodes =
      catMaybes $
        fmap
          ( \i -> do
              pos <- varM (fst i) gridMap
              let pres = 0
              return $ transformElement pos $ renderNode (fst i) (snd i)
          )
          (nodes iter)
      where
        gridMap = M.fromList (nodesPosition coord)
    styleLinks =
      concat $ catMaybes $
          ( \(l, (h, t, i)) -> do
              pos <- varM l posMap
              return $ catMaybes $
                zipWith
                  ( \m n -> do
                      return $ transformElement m $ renderLink l h t n
                  )
                  pos
                  i
          ) <$> links iter
      where
        posMap = M.fromList $ linksPosition coord 

styleSurfaces :: (Show (b Double), Target b a) => CoordinateGrid (TCoord a) Double -> Grid b Double -> [a]
styleSurfaces coord it =
  catMaybes $
    fmap
      ( \(n, (h, i)) -> do
          let paths = fmap (fmap (\l -> var l lEls)) h
              nodes = fmap (\(h, t, _) -> [(h, var h npos), (t, var t npos)]) (snd <$> paths)
          return $ renderSurface paths (concat nodes) i
      )
      (surfaces it)
  where
    lEls = M.fromList $ links it
    npos = M.fromList $ nodesPosition coord 

styleSurfacesSolve :: (PreSys b, Show (b Double), Show (SurfaceDomain b Double), Target b a) => FIteration (TCoord a) (NodeDomain b) (LinkDomain b) (Enviroment b) b Double -> [a]
styleSurfacesSolve iter@(Iteration l n e it p) =
  catMaybes $
    fmap
      ( \(n, (h, i)) -> do
          let paths = fmap (fmap (\l -> var l lEls)) h
              nodes = fmap (\(h, t, _) -> [(h, var h npos), (t, var t npos)]) (snd <$> paths)
              m = (\i -> (i, var i spos)) . fst <$>  (concat nodes)
          return $ renderSurfaceSolve m paths (concat nodes) i (renderSurface paths (concat nodes) i)
      )
      (surfaces it)
  where
    spos = M.fromList $ postprocess iter
    lEls = M.fromList $ links it
    npos = M.fromList $ nodesPosition p 

styleVolume :: (Show (b Double), Target b a) => CoordinateGrid (TCoord a) Double -> Grid b Double -> [a]
styleVolume coord it =
  catMaybes $
    fmap
      ( \(n, (h, i)) -> do
          let surfs = fmap (\(d, i) -> if d then fst $ var i lSurfs else fmap (first flip) $ fst $ var i lSurfs) h
              paths = fmap (fmap (\l -> var l lEls)) <$> surfs
              nodes = fmap (\(h, t, _) -> [(h, var h npos), (t, var t npos)]) $ (snd <$> concat paths)
          return $ renderVolume paths (concat nodes) i
      )
      (volumes it)
  where
    flip True = False
    flip False = True
    lEls = M.fromList $ links it
    lSurfs = M.fromList $ surfaces it
    npos = M.fromList $ nodesPosition coord

mergeStates :: (Traversable t1, Show b, Foldable t2) => t1 (Maybe b) -> t2 b -> t1 b
mergeStates i x = fst $ runState (traverse parse i) (F.toList x)

drawIter :: (TField a ~ Double, Target b a, Traversable (LinkDomain b), Traversable (NodeDomain b), PreSys b, Semigroup a, Show (b Double), Show (SurfaceDomain b Double)) => FIteration (TCoord a) (NodeDomain b) (LinkDomain b) (Enviroment b) b Double -> a
drawIter iter = statements $ nds <> lds <> styleSurfaces (position iter) (grid iter) <> styleVolume (position iter) (grid iter) <> styleSurfacesSolve iter
  where
    nds = styleNodes iter
    lds = styleLinks iter
    styleNodes it =
      catMaybes $
        fmap
          ( \i -> do
              pos <- varM (fst i) gridMap
              pres <- varM (fst i) (M.fromList (pressures it))
              let nstate = mergeStates (constrained (snd i)) pres
              return $ transformElement pos $ (renderNode (fst i) (snd i) <> renderNodeSolve nstate (fst i) (snd i))
          )
          (nodes (grid it))
      where
        gridMap = M.fromList (nodesPosition $ position it)
    styleLinks it =
      concat $ catMaybes $
        fmap
          ( \(l, (h, t, i)) -> do
              pos <- varM l posMap
              flow <- varM l (M.fromList (flows it))
              let nstate = mergeStates (lconstrained i) flow 
              return $ catMaybes $
                zipWith
                  ( \m n -> do
                      return $ transformElement m $ renderLink l h t n <> renderLinkSolve nstate l h t n
                  )
                  pos
                  i
          )
          (links (grid it))
      where
        posMap = M.fromList $ linksPosition (position it)

drawIterGraph :: Target sys a => CoordinateGrid V3 Double ->  Grid sys Double -> a
drawIterGraph coord iter = statements $ nds <> lds
  where
    nds = styleNodes iter
    lds = styleLinks iter
    styleNodes it =
      catMaybes $
        fmap
          ( \i -> do
              return $ renderNode (fst i) (snd i)
          )
          (nodes (it))
      where
        gridMap = (M.fromList (nodesPosition coord))
    --styleLinks :: Iteration Double -> [Mecha.Solid]
    styleLinks it =
      concat $ catMaybes $
        fmap
          ( \(l, (h, t, i)) -> do
              return $ catMaybes $
                  ( \n -> do
                      return $ renderLink l h t n
                  ) <$> i
          )
          (links (it))

upgradeGrid ::
  (Multi.MultiLinear (Ang t) , Additive t , Show1 t , Show1 (Ang t) , Show a, RealFloat a, Show1 f , Coord f t) =>
  Int ->
  Int ->
  Grid f a ->
  (CoordinateGrid t a, Errors [(Int, Int, String, a)] [(Int, Int, (t a, Ang t a))])
upgradeGrid ni li a = (CoordinateGrid  (M.toList linksPos) (M.toList nodesPos) [], err)
  where
    (err, (nodesPos, linksPos)) =
      runState
        ( do
            modify (<> (M.singleton ni (zero, rot 0 0), mempty))
            let niel = var ni nmap
                nels = fmap snd $ thisElement (fst niel) (snd niel)
                oi = var li nels
            i <- locateGrid lmap nmap ni oi li (Left $ var li lmap)
            j <- mapM (\(inx, ie) -> locateGrid lmap nmap ni ie inx (Left $ var inx lmap)) (filter ((/= li) . fst) $ M.toList nels)
            return $ mappend <$> i <*> (foldr (liftA2 mappend) (pure []) j)
        )
        (mempty, mempty)
    lmap = M.fromList (links a)
    nmap = M.fromList (findNodesLinks a $ (nodes a))

recurse :: (Monad m, Ord b1, Show b1, Show b2, Show c) => (b1 -> Either ([b1], b2) (b1, b1, c) -> a) -> b1 -> Either ([b1], b2) (b1, b1, c) -> ReaderT (Map b1 ([b1], b2), Map b1 (b1, b1, c)) (StateT (Set b1, Set b1) m) [a]
recurse render ni r@(Right l@(h, t, e)) = do
  lift $ modify (<> (S.empty, S.singleton ni))
  i <- fst <$> lift get
  linkmap <- fst <$> ask
  let nexts = S.toList $ S.difference (S.fromList [h, t]) i
  ti <- mapM (\i -> recurse render i . Left . flip var linkmap $ i) nexts
  return $ render ni r : concat ti
recurse render ni r@(Left n@(lks, e)) = do
  lift $ modify (<> (S.singleton ni, S.empty))
  s <- snd <$> lift get
  nodemap <- snd <$> ask
  let nexts = S.toList $ S.difference (S.fromList lks) s
  ti <- mapM (\i -> recurse render i . Right . flip var nodemap $ i) nexts
  return $ render ni r : concat ti

findNodesLinks :: Grid b a -> [(Int, b1)] -> [(Int, ([Int], b1))]
findNodesLinks grid = fmap (\(i, n) -> (i, (var i nodeMapSet, n)))
  where
    nodeMapSet = fmap L.nub $ M.fromListWith mappend $ concat $ (\(l, (h, t, _)) -> [(h, [l]), (t, [l])]) <$> links grid
