{-# LANGUAGE GADTs,UnicodeSyntax,FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Diagram where

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

import Diagrams.Prelude hiding (trace,offset)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text (Text)


renderElem
  :: (Monad m ) =>
          (S.Set Int, (Double,(Int, Element Double)))
     -> StateT ([Double],S.Set Int, S.Set Int, M.Map Int (R2,Double)) m (Diagram SVG R2)
renderElem (s,(p,(n,Open i))) = return $ text (show n) # fontSizeL 0.1 # fc white <> circle 0.1 # fc green # lwL 0.04
renderElem (s,(p,(n,Tee (TeeConfig tc@[rl,b,rr] _ _ _ _)) )) =  return $ text (show n ) # fontSizeL 0.2 # fc black <> rotateBy (1/4) (triangle 0.4) # fc red # lwL 0.04
renderElem ite@((s,(p,(n,Sprinkler (Just (d,k))  _ _ _) ))) =  do
  ([maxf,minf],visited,visitedNode,_) <- get
  let
        nf = (k*sqrt p)/(maxf -minf)
        sp = text (show n ) # fontSizeL 0.2 # fc black <> circle 0.2 # opacity (0.3 + 0.7*nf) # fc darkblue # lwL 0.001 <> circle 0.21 #lwL 0.04 <> flabel <> plabel
        flabel = translate (r2 (0,-0.45)) (text (formatFloatN 2 (k*sqrt p) ++ " L/min") # fontSizeL 0.2 )
        plabel = translate (r2 (0,-0.70)) (text (formatFloatN 2 p ++ " kpa") # fontSizeL 0.2 )
  return sp




renderGrid
  :: ( Renderable Text b,
      Renderable (Path R2) b,
      Monad m ) =>
     M.Map
       Int
          (Double,(Int, Int, Int, [Element Double]))

     ->M.Map
        Int
          (S.Set Int, (Double,(Int, Element Double)))
     -> Int
     -> Double
     -> Either
          (Double,(Int, Int, Int, [Element Double])) (S.Set Int, (Double,(Int, Element Double)))
     -> StateT ([Double],S.Set Int, S.Set Int, M.Map Int (R2,Double)) m (Diagram b R2)
renderGrid lmap nmap l r (Right (s,(p,(n,Open i)))) = do
  visitNode n
  return $ rotateBy r $ text (show n) # fontSizeL 0.1 # fc white <> circle 0.1 # fc green # lwL 0.04

renderGrid lmap nmap l r ite@(Right (s,(p,(n,Sprinkler (Just (d,k))  _ _ _) ))) = do
    let h = case S.toList $ S.delete l  s of
          [h] -> h
          i -> error $ " empty link set " <> show ite
    visitNode n
    ([maxf,minf],visited,visitedNode,_) <- get
    let nf = (k*sqrt p)/(maxf -minf)
        sp = text (show n ) # fontSizeL 0.2 # fc black <> circle 0.2 # opacity (0.3 + 0.7*nf) # fc darkblue # lwL 0.001 <> circle 0.21 #lwL 0.04 <> flabel <> plabel
        flabel = translate (r2 (0,-0.45)) (text (formatFloatN 2 (k*sqrt p) ++ " L/min") # fontSizeL 0.2 )
        plabel = translate (r2 (0,-0.70)) (text (formatFloatN 2 p ++ " kpa") # fontSizeL 0.2 )
    g <- if S.member h visited then return mempty else maybe (return errorCross ) (renderGrid lmap nmap n 0) (Left <$> varM  h lmap)
    return $ rotateBy r $ sp <> g
  where

renderGrid lmap nmap l  r (Right (s,(p,(n,Tee (TeeConfig tc@[rl,b,rr] _ _ _ _)) )))
  | rl == l =  do
    visitNode n
    rre <- trav 0  rr
    be <- trav (-1/4) b
    return $ rotateBy r $   rotateBy (1/4) sp <>  be <>  rre
  | rr == l = do
    visitNode n
    rle  <- trav 0 rl
    be <-  trav (1/4) b
    return $  rotateBy r $  rotateBy (-1/4) sp <>  be <>  rle
  | b == l = do
    visitNode n
    rre <- trav (-1/4) rr
    rle  <- trav (1/4 ) rl
    return $ rotateBy r $  sp <> rre <>  rle
  where
    sp = text (show n ) # fontSizeL 0.2 # fc black <> rotateBy (1/4) (triangle 0.4) # fc red # lwL 0.04
    trav ri i =  do
        (_,visited,_,_) <- get
        if S.member i visited then return mempty else maybe (return errorCross) (renderGrid lmap nmap n ri) (Left <$> varM i lmap )

renderGrid lmap nmap n r ll@(Left (fs,(l,h,t,e)))
  | n == h =  do
    path fs t e
  | n == t = do
    path  (-fs) h (revElems e)
  | otherwise = error $ "wrong back element " <> show n  <> " " <> show ll
  where
    path f h e = do
        ([maxf,minf],visited,visitedNode,nodeMap) <- get
        let tn =  trans (var n nodeMap) (trans (0,r) $ foldr transElem (0,0) e)
            nf = abs f/(maxf-minf)
        g <- nextNode  tn h
        return $  rotateBy r $ foldr (renderLink (f,nf) l) g e
    nextNode  dn@(dist,a) h = do
        (i,visited,visitedNode,nodeMap) <- get
        visitLink l
        if  not $ S.member h visitedNode
          then do
            markNode h dn
            maybe (return errorCross) (renderGrid lmap nmap l 0) (Right <$> varM h nmap)
          else
              if  abs (distance (r2p dist)  (r2p (fst $ var h nodeMap))) < 1e-2
               then {-traceShow (show l <> " exact union point " <> show h <> " " <> show dist <> " == " <>  show (var h nodeMap))  $-}return mempty
               else traceShow (show l <> " non exact union point " <> show h <> " " <> show dist <> " /= " <>  show (var h nodeMap)) $ return errorCross

renderGrid lmap nmap  j r l = error $ "no match grid " <> show l


renderLink :: (Renderable Text b,
                      Renderable (Path R2) b ) =>
                      (Double,Double)
                      -> Int
                      -> Element Double
                      -> Diagram b R2
                      -> Diagram b R2
renderLink (f,nf) l t@(Tubo _ c _) o =  transformElem t  o <> (line <> foldr1 mappend (zipWith (\i j-> translate (r2 (c/2,(0.4 - 0.2*i ))) j ) [0..] [label , dlabel , llabel , flabel]))
  where
    label  = text (show l) # fontSizeL 0.2 # fc black
    dlabel  = text (show (1000* (fromJust $ diametroE t)) ++ " cm" ) # fontSizeL 0.2 # fc black
    llabel  = text (show (1000*  c ) ++ " cm" ) # fontSizeL 0.2 # fc black
    flabel  = text ((formatFloatN 2 f) ++ " L/min" ) # fontSizeL 0.2 # fc black
    line =  translate (r2 (c/2,0)) (if f > 0 then ahead else reflectX ahead )<> fromOffsets [realToFrac c * unitX ] # lwL 0.04# opacity (0.3 + 0.7*nf) # lc darkblue
renderLink f _ j@(Joelho _ _ _ _ ) l = joelho <> transformElem j  l
  where
    joelho = circle 0.1 # fc blue # lwL 0.04
renderLink f _ i j = j

angleElem = sum . fmap angleE


angleE :: Fractional b => Element a -> b
angleE (Joelho _ _ DRight _ ) = 1/4
angleE (Joelho _ _ DLeft _ ) = -1/4
angleE  i = 0

lengthE :: Element Double -> R2
lengthE (Tubo _ c _ ) = r2 (c,0)
lengthE i = 0

elemTrans t = (lengthE t , angleE t)
trans (l,a) (lo,ao) = (rotateBy a lo + l , a + ao)
transElem e =  trans (elemTrans e)

transformElem t o =  translate (lengthE t) (rotateBy (angleE t ) o)

revElems :: [Element a ] -> [Element a]
revElems = reverse .(fmap revElem)
  where
    revElem (Joelho i j DRight k)  =  (Joelho i j DLeft k)
    revElem (Joelho i j DLeft k)  =  (Joelho i j DRight k)
    revElem i = i


errorCross =  rotateBy (1/8) ( hrule 0.5 <> vrule 0.5 ) # lc red # lwL 0.08
ahead =   reflectX $ (rotateBy (1/8) (fromOffsets[0.20*unitX]) <>  reflectY (rotateBy (1/8) $fromOffsets[0.20*unitX] ))# lwL 0.04


p2r = r2 . unp2
r2p = p2 . unr2

visitNode n = modify (<> (mempty,mempty,S.singleton n,mempty))
visitLink n = modify (<> (mempty,S.singleton n,mempty,mempty))
markNode n c = modify (<> (mempty,mempty,mempty, M.singleton n c))

varM i j = case M.lookup i j of
              Nothing -> traceShow ("no var " <> show i ) Nothing
              i -> i
