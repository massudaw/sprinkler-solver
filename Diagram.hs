{-# LANGUAGE FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
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
import Language.Mecha.Types
import Language.Mecha.Solid


import Diagrams.Prelude hiding (trace,offset)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text (Text)



renderElemMecha  _ (_,(_,(_,Open i))) = color (0,1,0,1) $ sphere 0.1
renderElemMecha  _ (_,(_,(_,Tee (TeeConfig _ r i j _ ) ))) = color (1,0,0,1) $ rotateY (-pi/2) $ moveZ (-0.5*j) $ cone i (2*j) (2*j)
renderElemMecha  [maxf,minf] (_,(p,(_,Sprinkler (Just (d,k)) _ _ _))) = color (0,0,1,0.3 + 0.7*nf) $ sphere 0.15
  where
        nf = (k*sqrt p)/(maxf -minf)
renderElemMecha  _ i = error $ show i

renderLinkMecha (f,nf)  _ (Tubo (Just d)  c _ ) = color (0.2,0.2,1, 0.3 +0.7*nf) $ rotateY (pi/2) $ cylinder d c
renderLinkMecha _ _ (Joelho (Just d)  c _  _  ) = sphere d
renderLinkMecha _ _  i = sphere 0.05

renderElem
  ::
          [Double] -> (S.Set Int, (Double,(Int, Element Double)))
     ->  (Diagram SVG R2)
renderElem _ (s,(p,(n,Open i))) =  text (show n) # fontSizeL 0.1 # fc white <> circle 0.1 # fc green # lwL 0.04
renderElem _ (s,(p,(n,Tee (TeeConfig tc@[rl,b,rr] _ _ _ _)) )) =   text (show n ) # fontSizeL 0.2 # fc black <> rotateBy (1/4) (triangle 0.4) # fc red # lwL 0.04
renderElem [maxf,minf] ite@((s,(p,(n,Sprinkler (Just (d,k))  _ _ _) ))) =  sp
  where
        nf = (k*sqrt p)/(maxf -minf)
        sp = text (show n ) # fontSizeL 0.2 # fc black <> circle 0.2 # opacity (0.3 + 0.7*nf) # fc darkblue # lwL 0.001 <> circle 0.21 #lwL 0.04 <> flabel <> plabel
        flabel = translate (r2 (0,-0.45)) (text (formatFloatN 2 (k*sqrt p) ++ " L/min") # fontSizeL 0.2 )
        plabel = translate (r2 (0,-0.70)) (text (formatFloatN 2 p ++ " kpa") # fontSizeL 0.2 )
renderElem _ i = error $  show i

renderLinkSVG :: (Renderable Text b,
                      Renderable (Path R2) b ) =>
                      (Double,Double)
                      -> Int
                      -> Element Double
                      -> Diagram b R2
renderLinkSVG (f,nf) l t@(Tubo _ c _) =   (line <> foldr1 mappend (zipWith (\i j-> translate (r2 (c/2,(0.4 - 0.2*i ))) j ) [0..] [label , dlabel , llabel , flabel]))
  where
    label  = text (show l) # fontSizeL 0.2 # fc black
    dlabel  = text (show (1000* (fromJust $ diametroE t)) ++ " cm" ) # fontSizeL 0.2 # fc black
    llabel  = text (show (1000*  c ) ++ " cm" ) # fontSizeL 0.2 # fc black
    flabel  = text ((formatFloatN 2 f) ++ " L/min" ) # fontSizeL 0.2 # fc black
    line =  translate (r2 (c/2,0)) (if f > 0 then ahead else reflectX ahead )<> fromOffsets [realToFrac c * unitX ] # lwL 0.04# opacity (0.3 + 0.7*nf) # lc darkblue
renderLinkSVG f _ j@(Joelho _ _ _ _ )  =  joelho
  where
    joelho = circle 0.1 # fc blue # lwL 0.04
renderLinkSVG f _ i = mempty -- error $ show i


thisElement l (p,(n,Open i))  =  (0,0)
thisElement l (p,(n,Sprinkler _ _ _ _ ))  =  (0,0)
thisElement l (p,(n,Tee (TeeConfig [rl,b,rr] _ _ _ _)))
  | rl == l =  (0,1/4)
  | rr == l =   (0,-1/4)
  | b == l =   (0,0)


nextElement _ (p,(n,Open i))  =  []
nextElement l (s,(n,Sprinkler i _ _ _ ))  =  [DiagramElement 0 0  h]
  where h = case S.toList $ S.delete l  s of
          [h] -> h
          i -> error $ " empty link set " <> show n
nextElement l (p,(n,Tee (TeeConfig [rl,b,rr] _ _ _ _)) )
  | rl == l =  [DiagramElement 0 0 rr, DiagramElement 0 (-1/4) b]
  | rr == l =  [DiagramElement 0 0 rl, DiagramElement 0 (1/4) b]
  | b == l =  [DiagramElement 0 (-1/4) rr, DiagramElement 0 (1/4) rl]



class Target a where
  renderNode :: [Double] -> (S.Set Int,(Double,(Int,Element Double))) -> a
  renderLink ::  (Double,Double) ->  Int -> Element Double -> a
  errorItem :: a
  transformElement  :: (R2,Double) -> a -> a

instance Target (Diagram SVG R2) where
  renderNode = renderElem
  renderLink = renderLinkSVG
  errorItem = errorCross
  transformElement (r,a) = translate r . rotateBy a

instance Target Solid where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha
  errorItem = torus 0.2 0.1
  transformElement (r,a)= moveX (fst $ unr2 r) . moveY (snd $unr2 r) . rotateZ (a*2*pi)

renderGrid
  :: (Target a , Monad m)  =>
     M.Map
       Int
          (Double,(Int, Int, Int, [Element Double]))
     ->M.Map
        Int
          (S.Set Int, (Double,(Int, Element Double)))
     -> Int
     -> (R2,Double)
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
        let pos = (fst r + ri ,snd r + ai)
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
              if  abs (distance (r2p dist)  (r2p ( dpos $ var h visitedNode))) < 1e-2
               then {-traceShow (show l <> " exact union point " <> show h <> " " <> show dist <> " == " <>  show (var h nodeMap))  $-}return ()
               else traceShow (show l <> " non exact union point " <> show h <> " " <> show dist <> " /= " <>  show (var h visitedNode)) $ (markNode h (DiagramElement dist a errorItem))


-- etrans e = (dpos e,ddir e)

data DiagramElement a
  = DiagramElement
  { dpos :: R2
  , ddir :: Double
  , dele :: a
  }
instance Show (DiagramElement a ) where
  show (DiagramElement p d _ ) = show (p,d)

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
transElemr e =  trans (elemTrans e)
transEleml i e =  trans i (elemTrans e)

transformElem t o =  translate (lengthE t) (rotateBy (angleE t ) o)

revElems :: [Element a ] -> [Element a]
revElems = reverse .fmap revElem
  where
    revElem (Joelho i j DRight k)  =  (Joelho i j DLeft k)
    revElem (Joelho i j DLeft k)  =  (Joelho i j DRight k)
    revElem i = i


errorCross =  rotateBy (1/8) ( hrule 0.5 <> vrule 0.5 ) # lc red # lwL 0.08
ahead =   reflectX $ (rotateBy (1/8) (fromOffsets[0.20*unitX]) <>  reflectY (rotateBy (1/8) $fromOffsets[0.20*unitX] ))# lwL 0.04


p2r = r2 . unp2
r2p = p2 . unr2

markNode n c = modify (<> (mempty, M.singleton n c,mempty))
markLink n c = modify (<> (mempty,mempty,M.singleton n c))

varM i j = case M.lookup i j of
              Nothing -> traceShow ("no var " <> show i ) Nothing
              i -> i
