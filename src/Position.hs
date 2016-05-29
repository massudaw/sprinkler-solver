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
import Control.Monad.Trans.State
import Linear.V3
import Linear.V1
import Linear.Metric
import Linear.Matrix
import Linear.Vector
import Rotation.SO3 hiding (rotM)
import Control.Applicative
import Control.Monad.Trans.State
import qualified Data.Set as S
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Data.Semigroup

showErr (Other (Constant i)) = Left i
showErr (Pure i)  = Right i

subSp (i,b) (j,c) = (i ^-^ j, SO3 $   distribute (unSO3 c) !*! (unSO3  b )  )

nextS :: (Show (f Double),Coord f (V3 Double)) => Int -> [Int] -> f Double -> [(Int,(V3 Double,SO3 Double))]
nextS l p v  = fmap (\i -> (i,  nElement i p v )) $ filter (/=l) p

nextE :: (Show (f Double),Coord f (V3 Double)) => Int -> [Int] -> f Double -> [(Int,(V3 Double,SO3 Double))]
nextE l p v  = fmap (\i -> (i,  subSp  (nElement i p v) ini ) ) $ filter (/=l) p
  where ini =  tElement l p v


-- tElement :: Show (f Double) => Int -> f Double -> TCoord (f Double)
tElementInfer l i e = fmap snd . M.lookup l . thisElement i $ e
tElement l i e = snd . justError (" no element " <> show l <> " in " <> show e  ) . M.lookup l . thisElement i $ e
nElement l i e = case  justError (" no element " <> show (l,e) ) . M.lookup l . thisElement i $ e of
            (0,v) -> untrans (0,so3 (V3 pi 0 0)) v
            (1,v) -> untrans (0,so3 (V3 0 pi 0)) v
            (2,v) -> untrans (0,so3 (V3 0 0 pi )) v

angDist i j = unRot $ SO3 $ distribute $ (distribute $ unSO3 i) !*! ( unSO3 j)
instance PreCoord (V3 Double) where
  type Ang (V3 Double) = SO3 Double
  dist (i,ir) (j,jr) = (distance (i) (j) ,norm $ angDist ir jr)
  trans (l,i) (lo,a) = ( l + unSO3 i !* lo  , SO3 $ unSO3 i !*!  unSO3 a )
  untrans (l,i) (lo,a) = ( l ^-^  unSO3 i !* lo  , SO3 $ unSO3 i !*! distribute (unSO3 a )  )

so3 :: Floating a => V3 a -> SO3 a
so3 = SO3 . rotM

rot (V3 ix iy iz) = rotZ (V1 iz) !*! rotY (V1  iy)  !*! rotX (V1  ix)

rotD (V3 ix iy iz) = distribute (rotZ (V1 iz)) !*! (distribute (rotY (V1  iy))  !*! distribute (rotX (V1  ix)))

rot132  (V3 ix iy iz) =  (distribute (rotY (V1  iy))  !*! distribute (rotZ (V1 iz)) !*! distribute (rotX (V1  ix)))
unRot = unRot123

opi i = i * 2 *pi
upi i = i/(2 *pi)

unrot = unRot . SO3 . distribute . unSO3

locateGrid
  :: (SO3 Double ~ (Ang a) , a ~ V3 Double, Coord f a, Show (f Double),Show (Ang a), Show a,  Num a, Monad m) =>
     M.Map Int (Int, Int, [ f Double ])
     -> M.Map Int ([Int], f Double )
     -> Int
     -> (a, Ang a)
     -> Int
     -> Either
          (Int, Int, [f Double])
          ([Int], f Double)
     -> StateT (M.Map Int (a, Ang a), M.Map Int [(a, Ang a)]) m (Errors [(Int,Int,String,Double)] [(Int,Int,(a,Ang a))])
locateGrid lmap nmap l r n (Right oe@(s,e)) = do
  let
      t = tElement  l  s e
      rnew = trans r t
  modify (<> (M.singleton n rnew,mempty))
  let trav ne@(i,coo)  =  do
        let pos = trans rnew coo
        locateGrid lmap nmap n  pos i (Left $ var i lmap )
  l <- mapM trav  (nextS l s e)
  return (foldl (liftA2 mappend) (pure []) l )

locateGrid lmap nmap n r l ll@(Left (hn,tn,e))
  | n == hn =  do
    (i,err) <- path tn e
    modify (<> (mempty ,M.singleton l i))
    return err
  | n == tn = do
    (i,err) <- revpath hn ( e)
    modify (<> (mempty ,M.singleton l i))
    return err
    return (pure [])
  | otherwise = error $ "wrong back element " <> show n  <> " " <> show ll
  where

    revpath  nn e = do
      let
        es =justError "no element" .  M.lookup hn . M.fromList . nextE tn [hn,tn] <$> e
        sn =  scanr transElemr r es
      err <- nextNode  (head sn) nn
      return (init $ reverse sn,err)
    path  nn e = do
      let
        es =justError "no element" .  M.lookup tn . M.fromList . nextE hn [hn,tn] <$> e
        sn =   scanl transEleml r es
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
            case uncurry (tElementInfer l ) (var nn  nmap)of
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
              Nothing ->  return $ pure [(l,nn,(0 ,SO3 $ (unSO3 $ snd npos ) !*! distribute (unSO3 $ snd pos )))]




rotM = rotD

transElemr e =  flip untrans e
transEleml i e =  trans i e

drawGrid
  :: (Show (sys Double), Target sys a, TCoord a ~ V3 Double,
      Ang (TCoord a) ~ SO3 Double) =>
     Grid sys Double -> a
drawGrid iter = statements  $ styleNodes iter <> styleLinks iter <> {- styleSurfaces iter <> -}styleVolume iter
  where
    styleNodes  it = catMaybes $ fmap (\i -> do
            pos <- varM (fst i) gridMap
            let pres = 0
            return $ transformElement  pos $ renderNode  (fst i ) (snd i) ) (nodes it)
      where
        gridMap = (M.fromList (nodesPosition $ it))

    styleLinks it = concat $ catMaybes $  fmap (\(l,(h,t,i))  -> do
                pos <- varM l  posMap
                return $ catMaybes $ zipWith3 (\m ix n ->  do
                  return $ transformElement m $ renderLink   ix  l n ) pos  [0..] i ) (links (it))
      where
        posMap = M.fromList $ linksPosition (it)

styleSurfaces it = catMaybes $  fmap (\(n,(h,i))  -> do
                let paths = fmap (fmap (\l -> var l lEls)) h
                    nodes = fmap (\(h,t,_)->  [(h,var h npos),(t,var t npos)]) (snd <$>paths)
                return $ renderSurface paths (concat nodes)  i ) (surfaces it)
      where
        lEls = M.fromList $ links it
        npos = M.fromList $ nodesPosition it

styleSurfacesSolve iter@(Iteration l n e it) = catMaybes $  fmap (\(n,(h,i))  -> do
                let paths = fmap (fmap (\l -> var l lEls)) h
                    nodes = fmap (\(h,t,_)->  [(h,var h npos),(t,var t npos)]) (snd <$>paths)
                    m  =(\i -> (i,var i  spos)) . fst <$> L.nub (concat nodes)
                return $ renderSurfaceSolve m paths (concat nodes)  i (renderSurface paths (concat nodes) i)) (surfaces it)
      where
        spos = M.fromList $ postprocess iter

        lEls = M.fromList $ links it
        npos = M.fromList $ nodesPosition it


styleVolume it = catMaybes $  fmap (\(n,(h,i))  -> do
                let surfs = fmap (\(d,i) -> if d then fst $ var i lSurfs else fmap (first flip ) $ fst $ var i lSurfs ) h
                    paths = fmap (fmap (\l -> var l lEls)) <$> surfs
                    nodes = fmap (\(h,t,_)->  [(h,var h npos),(t,var t npos)]) $ (snd <$> concat paths)
                return $ renderVolume paths (concat nodes)  i ) (volumes it)
      where
        flip True = False
        flip False = True
        lEls =  M.fromList $ links it
        lSurfs =  M.fromList $ surfaces it
        npos = M.fromList $ nodesPosition it



mergeStates i x = fst $ runState( traverse parse i) (F.toList x)

drawIter iter = statements $ nds <> lds <> styleSurfaces (grid iter) <> styleVolume (grid iter) <> styleSurfacesSolve iter
  where
    nds = styleNodes iter
    lds = styleLinks iter
    styleNodes  it = catMaybes $ fmap (\i -> do
            pos <- varM (fst i) gridMap
            pres <- varM (fst i) (M.fromList (pressures it))
            let nstate = mergeStates (constrained (snd i))  pres
            return $ transformElement  pos $ (renderNode   (fst i) (snd i) <> renderNodeSolve nstate (fst i) (snd i) )) (nodes (grid it))
      where
        gridMap = (M.fromList (nodesPosition $ grid it))

    styleLinks it = concat $ catMaybes $  fmap (\(l,(h,t,i))  -> do
                pos <- varM l  posMap
                return $ catMaybes $ zipWith3 (\m ix n ->  do
                  let flow = 0
                  return $ transformElement m $ renderLink   ix  l n ) pos  [0..] i ) (links (grid it))
      where
        posMap = M.fromList $ linksPosition (grid it)


drawIterGraph  iter = statements $ nds <> lds
  where nds = styleNodes iter
        lds = styleLinks iter
        styleNodes  it = catMaybes $ fmap (\i -> do
                return $ renderNode  (fst i) (snd i) ) (nodes (it))
          where
                gridMap = (M.fromList (nodesPosition $ it))

        --styleLinks :: Iteration Double -> [Mecha.Solid]
        styleLinks it = concat $ catMaybes $  fmap (\(l,(h,t,i))  -> do
                    return $ catMaybes $ zipWith3 (\m ix n ->  do
                      return $ renderLink   ix  l n ) [0..] [0..] i ) (links (it))


upgradeGrid :: (Show (f Double) , Coord f (V3 Double)) => Int -> Int -> Grid f Double -> (Grid  f Double,Errors [(Int,Int,String,Double)] [(Int,Int,(V3 Double ,SO3 Double))])
upgradeGrid ni li a = (a {nodesPosition = M.toList nodesPos, linksPosition = M.toList linksPos},err)
  where
    (err,(nodesPos,linksPos)) =  runState (do
          modify (<> (M.singleton ni (0,so3 0), mempty))
          let
            niel = var ni nmap
            nels = fmap snd $ thisElement (fst niel) (snd niel)
            oi = var li nels
          i <- locateGrid lmap nmap ni oi  li (Left $ var li lmap )
          j <- mapM (\(inx,ie) -> locateGrid lmap nmap ni ie inx (Left $ var inx lmap) ) (filter ((/=li).fst) $ M.toList nels)
          return $ mappend <$> i <*> (foldr (liftA2 mappend ) (pure []) j))  (mempty,mempty)
    lmap = M.fromList (links a)
    nmap = M.fromList (findNodesLinks a $   (nodes a) )

recurse render ni r@(Right l@(h,t,e)) = do
  lift $ modify (<> (S.empty,S.singleton ni))
  i <- fst <$> lift  get
  linkmap <- fst <$> ask
  let nexts = S.toList $ S.difference (S.fromList [h,t]) i
  ti <- mapM (\i -> recurse render i . Left . flip var linkmap $ i ) nexts
  return $ render ni r  : concat ti
recurse render ni r@(Left n@((lks,e))) = do
  lift $ modify (<>(S.singleton ni,S.empty))
  s <- snd <$> lift  get
  nodemap <- snd <$> ask
  let nexts = S.toList $ S.difference (S.fromList lks)  s
  ti <- mapM (\i -> recurse render i . Right . flip var nodemap $ i ) nexts
  return $ render ni r : concat ti


findNodesLinks grid = fmap (\(i,n) -> (i,(var i nodeMapSet,n)))
    where nodeMapSet = fmap L.nub $ M.fromListWith mappend $ concat $ (\(l,(h,t,_)) -> [(h,[l ]),(t,[l ])]) <$> links grid
