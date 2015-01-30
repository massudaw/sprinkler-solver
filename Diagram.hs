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

import Data.Traversable (traverse)

import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text




renderGrid
  :: ( Renderable Diagrams.TwoD.Text.Text b,
      Renderable (Diagrams.Prelude.Path R2) b, Show t,
      Monad m ) =>
     M.Map
       Int
       (Either
          (Double,(Int, Int, Int, [Element Double])) (S.Set Int, (Int, Element t)))
     -> Int
     -> Double
     -> Either
          (Double,(Int, Int, Int, [Element Double])) (S.Set Int, (Int, Element t))
     -> StateT ([Double],S.Set Int, S.Set Int, M.Map Int (P2,Double)) m (Diagram b R2)
renderGrid env l r (Right (s,(n,Open i))) = do
  visitNode n
  return $ rotateBy r $ text (show n) # fontSizeL 0.1 # fc white <> circle 0.1 # fc green # lwL 0.04

renderGrid env l r (Right (s,(n,Sprinkler _ _ _ _) )) = do
    let [h] = S.toList $ S.delete l  s
    visitNode n
    (i,visited,visitedNode,_) <- get
    g <- if S.member h visited then return mempty else renderGrid env n 0 (var  h env)
    return $ rotateBy r $ sp <> g
  where
    sp = text (show n ) # fontSizeL 0.2 # fc black <> circle 0.2 # fc yellow # lwL 0.04

renderGrid env l  r (Right (s,(n,Tee (TeeConfig tc@[rl,b,rr] _ _ _ _)) ))
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
        if S.member i visited then return mempty else renderGrid env n ri (var i env)

renderGrid env n r (Left (fs,(l,h,t,e)))
  | n == h =  do
    path fs t e
  | n == t = do
    path  (-fs) h (revElems e)
  where
    path f h e = do
        ([maxf,minf],visited,visitedNode,nodeMap) <- get
        let dist =  translate (p2r ( rotateBy (r + snd (var n nodeMap)) ( L.foldr offset (p2 (0,0)) e))) (fst $ var n nodeMap)
            nf = abs f/(maxf-minf)
        g <- nextNode (angle e) dist h
        return $  rotateBy r $ foldr (renderLink (f,nf)) g e
    nextNode  a dist h = do
        (i,visited,visitedNode,nodeMap) <- get
        visitLink l
        if  not $ S.member h visitedNode
          then do
            markNode h (dist, r + a + (snd $ var n nodeMap) )
            renderGrid env l 0 (var h env)
          else
              if  abs (distance dist  (fst $ var h nodeMap)) < 1e-2
               then {-traceShow (show l <> " exact union point " <> show h <> " " <> show dist <> " == " <>  show (var h nodeMap))  $-}return mempty
               else traceShow (show l <> " non exact union point " <> show h <> " " <> show dist <> " /= " <>  show (var h nodeMap)) $ return errorCross

    renderLink (f,nf) t@(Tubo _ c _) o =  offset t  o <> (line <> label <> dlabel <> llabel <> flabel)
      where label = translate (r2 (c/2,0.12)) ( text (show l)) # fontSizeL 0.2 # fc black
            dlabel = translate (r2 (c/2,-0.15)) ( text (  show (1000* (fromJust $ diametroE t)) ++ " cm" )) # fontSizeL 0.2 # fc black
            llabel = translate (r2 (c/2,-0.40)) ( text (  show (1000*  c ) ++ " cm" )) # fontSizeL 0.2 # fc black
            flabel = translate (r2 (c/2,0)) (if f > 0 then ahead else reflectX ahead )<> translate (r2 (c/2,-0.65)) ( text (  (formatFloatN 2 f) ++ "L/min" )) # fontSizeL 0.2 # fc black
            line = fromOffsets [realToFrac c * unitX ] # lwL 0.04# opacity (0.3 + 0.7*nf) # lc darkblue
    renderLink f j@(Joelho _ _ _ _ ) l = joelho <> offset j  l
     where joelho = circle 0.1 # fc blue # lwL 0.04
    renderLink f i j = j
    offset (Tubo _ c _) o =  translate (r2 (c,0)) o
    offset (Joelho _ _ DRight  _ ) l = rotateBy (1/4) l
    offset (Joelho _ _ DLeft _ ) l = rotateBy (-1/4) l
    offset i j = j
    angle = sum . fmap angle
      where
        angle (Joelho _ _ DRight _ ) = 1/4
        angle (Joelho _ _ DLeft _ ) = -1/4
        angle  i = 0
renderGrid i j r l = error $ show l


revElems :: [Element a ] -> [Element a]
revElems = reverse .(fmap revElem)
  where
    revElem (Joelho i j DRight k)  =  (Joelho i j DLeft k)
    revElem (Joelho i j DLeft k)  =  (Joelho i j DRight k)
    revElem i = i


errorCross =  rotateBy (1/8) ( hrule 0.5 <> vrule 0.5 ) # lc red # lwL 0.08
ahead =   reflectX $ (rotateBy (1/8) (fromOffsets[0.20*unitX]) <>  reflectY (rotateBy (1/8) $fromOffsets[0.20*unitX] ))# lwL 0.04


p2r = r2 . unp2

visitNode n = modify (<> (mempty,mempty,S.singleton n,mempty))
visitLink n = modify (<> (mempty,S.singleton n,mempty,mempty))
markNode n c = modify (<> (mempty,mempty,mempty,traceShowId $ M.singleton n c))
