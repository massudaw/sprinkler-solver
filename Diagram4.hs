{-# LANGUAGE FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Diagram4 where

import Grid
import Debug.Trace
import Lint
import Data.Maybe
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

import qualified Language.Mecha.Types as Mecha
import qualified Language.Mecha.Solid as Mecha


import Diagrams.Prelude.ThreeD
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text (Text)

import Control.Lens hiding(transform)

import Data.Void

renderElemMecha  _ (_,(_,(_,Open i))) = Mecha.color (0,1,0,1) $ Mecha.sphere 0.1
renderElemMecha  _ (_,(_,(_,Reservatorio _ _ i))) = Mecha.color (1,1,1,1) $ Mecha.sphere 0.5
renderElemMecha  _ (_,(_,(_,Tee (TeeConfig _ r i j _ ) ))) = Mecha.color (1,0,0,1) $ Mecha.rotateY (-pi/2) $ Mecha.moveZ (-0.5*j) $ Mecha.cone i (2*j) (2*j)
renderElemMecha  [maxf,minf] (_,(p,(_,Sprinkler (Just (d,k)) _ _ _))) = Mecha.color (0,0,1,0.3 + 0.7*nf) $ Mecha.sphere 0.15
  where
        nf = (k*sqrt p)/(maxf -minf)
renderElemMecha  _ i = error $ show i

renderLinkMecha (f,nf)  _ (Tubo (Just d)  c _ ) = Mecha.color (0.2,0.2,1, 0.3 +0.7*nf) $ Mecha.rotateY (pi/2) $ Mecha.cylinder d c
renderLinkMecha _ _ (Joelho (Just d)  c _  _  ) = Mecha.sphere d
renderLinkMecha _ _  i = Mecha.sphere 0.05

thisElement l (p,(n,Open i))  =  (0,0)
thisElement l (p,(n,Reservatorio _ _ i))  =  (0,0)
thisElement l (p,(n,Sprinkler _ _ _ _ ))  =  (0,0)
thisElement l (p,(n,Tee (TeeConfig [rl,b,rr] _ _ _ _)))
  | rl == l =  (0,(0,0,1/4))
  | rr == l =   (0,(0,0,-1/4))
  | b == l =   (0,0)


instance Num R3 where
  fromInteger int = r3 (i,i,i)
    where i = fromIntegral int
  r1 + r2 = r3 (r1 ^. _x + r2 ^. _x,r1 ^. _y + r2 ^. _y,r1 ^. _z + r2 ^. _z)

nextElement _ (p,(n,Open i))  =  []
nextElement _ (p,(n,Reservatorio _ _ i))  =  []
nextElement l (s,(n,Sprinkler i _ _ _ ))  =  [DiagramElement 0 0  h]
  where h = case S.toList $ S.delete l  s of
          [h] -> h
          i -> error $ " empty link set " <> show n
nextElement l (p,(n,Tee (TeeConfig [rl,b,rr] _ _ _ _)) )
  | rl == l =  [DiagramElement 0 0 rr, DiagramElement 0 (0,0,-1/4) b]
  | rr == l =  [DiagramElement 0 0 rl, DiagramElement 0 (0,0,1/4) b]
  | b == l =  [DiagramElement 0 (0,0,-1/4) rr, DiagramElement 0 (0,0,1/4) rl]



class Target a where
  renderNode :: [Double] -> (S.Set Int,(Double,(Int,Element Double))) -> a
  renderLink ::  (Double,Double) ->  Int -> Element Double -> a
  errorItem :: a
  transformElement  :: (R3,(Double,Double,Double)) -> a -> a

instance Target  Mecha.Solid where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha
  errorItem = Mecha.torus 0.2 0.1
  transformElement (r,(ax,ay,az))= Mecha.moveX (r ^. _x) . Mecha.moveY (r ^. _y) . Mecha.moveZ (r ^. _z) . Mecha.rotateX (ax *2*pi) . Mecha.rotateY (ay *2*pi) . Mecha.rotateZ (az *2*pi)

{-locateGrid
  :: (Target  a , Monad m)  =>
     M.Map
       Int
          (Double,(Int, Int, Int, [Element Double]))
     ->M.Map
        Int
          (S.Set Int, (Double,(Int, Element Double)))
     -> Int
     -> (R3,(Double,Double,Double))
     -> Either
          (Double,(Int, Int, Int, [Element Double])) (S.Set Int, (Double,(Int, Element Double)))
     -> StateT ([Double], M.Map Int (DiagramElement a ), M.Map Int [DiagramElement a]) m ()-}
locateGrid lmap nmap l r (Right oe@(s,(e@(n,_)))) = do
  let
      t = thisElement l (s,e)
  modify (<> (M.singleton n (DiagramElement (fst r + fst t) (snd r + snd t) 0)))
  let trav (ri,ai) i =  do
        let pos = (fst r + ri ,snd r  + ai)
        maybe (return ()) (locateGrid lmap nmap n pos ) (Left <$> varM i lmap )
  mapM (\(DiagramElement r a i) -> trav (r,a) i) (nextElement l (s,e))
  return ()
locateGrid lmap nmap n r ll@(Left (l,h,t,e))
  | n == h =  do
    path t e
  | n == t = do
    path  h (revElems e)
  | otherwise = error $ "wrong back element " <> show n  <> " " <> show ll
  where
    path  h e = do
        let
            sn = scanl transEleml r e
            lk = fmap (\(p,a) -> DiagramElement   p a 0) sn
        nextNode  (last sn) h
        return ()
    nextNode  pos@(dist ,a) h = do
        (visitedNode) <- get
        if  not $ M.member h visitedNode
          then do
            maybe (return () ) (locateGrid lmap nmap l pos) (Right <$> varM h nmap)
          else
              if  distance (r2p dist)  (r2p ( dpos $ var h visitedNode)) < 1e-2
               then {-traceShow (show l <> " exact union point " <> show h <> " " <> show dist <> " == " <>  show (var h nodeMap))  $-}return ()
               else traceShow (show l <> " non exact union point " <> show h <> " " <> show dist <> " /= " <>  show (var h visitedNode)) $ (return ())



renderGrid
  :: (Target  a , Monad m)  =>
     M.Map
       Int
          (Double,(Int, Int, Int, [Element Double]))
     ->M.Map
        Int
          (S.Set Int, (Double,(Int, Element Double)))
     -> Int
     -> (R3,(Double,Double,Double))
     -> Either
          (Double,(Int, Int, Int, [Element Double])) (S.Set Int, (Double,(Int, Element Double)))
     -> StateT ([Double], M.Map Int (DiagramElement a ), M.Map Int [DiagramElement a]) m ()
renderGrid lmap nmap l r (Right oe@(s,(p,e@(n,_)))) = do
  (i,_,_) <- get
  let sp = renderNode i oe
      t = thisElement l (s,e)
  markNode n (DiagramElement (fst r + fst t) (snd r + snd t) sp)
  let trav (ri,ai) i =  do
        (_,_,visited) <- get
        let pos = (fst r + ri ,snd r  + ai)
        if M.member i visited then return () else maybe (markLink i ([DiagramElement (fst pos) (snd pos) errorItem])) (renderGrid lmap nmap n pos ) (Left <$> varM i lmap )
  mapM (\(DiagramElement r a i) -> trav (r,a) i) (nextElement l (s,e))
  return ()
renderGrid lmap nmap n r ll@(Left (fs,(l,h,t,e)))
  | n == h =  do
    path fs t e
  | n == t = do
    path  (-fs) h (revElems e)
  | otherwise = error $ "wrong back element " <> show n  <> " " <> show ll
  where
    path f h e = do
        ([maxf,minf],_,_) <- get
        let
            nf = abs f/(maxf-minf)
            sn = scanl transEleml r e
            lk = zipWith (\(p,a) j -> DiagramElement   p a j) sn (renderLink (f,nf) l <$>  e)
        markLink l lk
        nextNode  (last sn) h
        return ()
    nextNode  pos@(dist ,a) h = do
        (i,visitedNode,visited) <- get
        if  not $ M.member h visitedNode
          then do
            maybe (markNode h (DiagramElement dist a errorItem)) (renderGrid lmap nmap l pos) (Right <$> varM h nmap)
          else
              if  distance (r2p dist)  (r2p ( dpos $ var h visitedNode)) < 1e-2
               then {-traceShow (show l <> " exact union point " <> show h <> " " <> show dist <> " == " <>  show (var h nodeMap))  $-}return ()
               else traceShow (show l <> " non exact union point " <> show h <> " " <> show dist <> " /= " <>  show (var h visitedNode)) $ (markNode h (DiagramElement dist a errorItem))


-- etrans e = (dpos e,ddir e)

data DiagramElement a
  = DiagramElement
  { dpos :: R3
  , ddir :: (Double,Double,Double)
  , dele :: a
  }
instance Show (DiagramElement a ) where
  show (DiagramElement p d _ ) = show (p,d)



r2p = p3 . unr3

angleE :: Fractional a => Element a -> (a,a,a)
angleE (Joelho _ _ DRight _ ) = (0,0,1/4)
angleE (Joelho _ _ (DUp r) _ ) = (1/4,0,r)
angleE (Joelho _ _ (DDown r) _ ) = (-1/4,0,r)
angleE (Joelho _ _ DLeft _ ) = (0,0,-1/4)
angleE  i = (0,0,0)

lengthE :: Element Double -> R3
lengthE (Tubo _ c _ ) = r3 (c,0,0)
lengthE i = 0

sumAngle (ix,iy,iz)(ax,ay,az) =(ix+ax,iy+ay,az +iz)

elemTrans t = (lengthE t , angleE t)
trans (l,i@(ix,iy,iz)) (lo,a@(ax,ay,az)) = ((transform (aboutX (ix @@ turn)) . transform (aboutY (iy @@ turn))  . transform (aboutZ (iz @@ turn))) lo + l , i + a )
transElemr e =  trans (elemTrans e)
transEleml i e =  trans i (elemTrans e)


revElems :: Num a => [Element a ] -> [Element a]
revElems = reverse .fmap revElem
  where
    revElem (Joelho i j DRight k)  =  (Joelho i j DLeft k)
    revElem (Joelho i j DLeft k)  =  (Joelho i j DRight k)
    revElem (Joelho i j (DUp r) k)  =  (Joelho i j (DDown (-r)) k)
    revElem (Joelho i j (DDown r) k)  =  (Joelho i j (DUp (-r)) k)
    revElem i = i



markNode n c = modify (<> (mempty, M.singleton n c,mempty))
markLink n c = modify (<> (mempty,mempty,M.singleton n c))

varM i j = case M.lookup i j of
              Nothing -> traceShow ("no var " <> show i ) Nothing
              i -> i
