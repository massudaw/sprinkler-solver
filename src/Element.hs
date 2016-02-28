{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
{-# LANGUAGE TupleSections,DeriveFunctor,DeriveFoldable #-}
module Element where

import Position hiding (rotM)
import Hydraulic
import qualified Position as P
import Control.Lens ((^.))
import Tee hiding (ktubo)
import Data.Monoid
import Data.Functor.Identity
import qualified Data.Map as M
import GHC.Stack
import Domains
import Position
import Control.Monad
import Linear.V3
import Linear.Matrix
import Linear.Vector
import Control.Arrow
import Rotation.SO3
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as F

import Linear.V3
import Rotation.SO3



instance PreSys Element  where
  type NodeDomain Element = Identity
  type LinkDomain Element= Identity
  revElem (Joelho i j b k) = Joelho i j (-b) k
  revElem (Turn i ) = Turn i
  revElem i = i


instance Coord Element (V3 Double) where
  nextElement  = nextS
  thisElement l i = (\j-> (0,SO3 $ P.rotM $ fmap opi $ (V3 0 0 j))) $ this l i
    where
      this l e  = justError "no el" $ M.lookup l  (M.fromList ( els $ first F.toList  e))
        where
          els (_,(_,Tee (TeeConfig [rl,b,rr] _ _ _ _) _ ))
            =  [(rl,1/4),(rr,-1/4),(b,0)]
          els ([a,b],i)
            =  [(a,0),(b,1/2)]
          els ([a],i)
            =  [(a,0)]
  elemTrans t = (lengthE t , angleE t)
    where
      angleE  = SO3 . P.rotM . (\i-> opi i ) . angE
        where
          angE :: Fractional a => Element a -> V3 a
          angE (Joelho _ _ r _ ) = r3 (0,0,r)
          angE (Turn c) = r3 (c,0,0)
          angE  i = r3 (0,0,0)


      lengthE :: Num a => Element a -> V3 a
      lengthE (Tubo _ c _ ) = r3 (c,0,0)
      lengthE i = 0
      r3 (x,y,z) = V3 x y z



pipeElement v e | v < 0 = negate $ pipeElement (abs v) e
pipeElement v (Bomba  ((pn,vn)) (Poly l ) ) = negate $ (*pn) $ (/100)  $foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*(100*v/vn)**p
-- pipeElement v e@(Resistive k p)  = k*v**p
pipeElement v e@(Tubo _ _ _)  = (ktubo e)*v**1.85
pipeElement v e@(Joelho _ _ _ _)  = (ktubo e)*v**1.85
pipeElement v e@(Perda __ _ _)  = (ktubo e)*v**1.85
pipeElement v (Turn _)   = 0


signedFlow :: (Show a,Floating a )=> Grid Element a -> M.Map Int a ->M.Map Int (M.Map Int a)
signedFlow g v = M.fromList $  fmap (\(i,_) ->  (i,) $ M.fromList $ ( ( sumn $ flipped i $ links g) ++   ((suma $ correct i $ links g))) ) (nodesFlow g)
  where flipped i=  filter (\(_,h,t,_) -> h == i )
        correct i= filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> (li,var li v ))
        sumn =  fmap (\(li,_,_,_) ->  (li,negate $ var li v))

jacobianContinuity :: (Show a,Ord a,Floating a )=> Grid Element a -> M.Map Int a -> M.Map Int a -> [a]
jacobianContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $ filter (not . isReservoir . snd) $ nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,h,t,_) -> h == i )
        correct i= suma . filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        nflow i e = genFlow (var i pm) e
        genFlow _ (Open i ) = i
        genFlow _ (Tee _ _ ) = 0
        genFlow idf (Sprinkler (Just (_,k)) _ _ _) = k*sqrt(abs idf)
        genFlow _ (Reservatorio _  ) = errorWithStackTrace "reservatorio doesn't preserve flow"

varn h = maybe 0 id .  M.lookup h
varr3 h = maybe (V3 0 0  0)  id .  M.lookup h

-- Generic Solver | Node + Head Method
jacobianNodeHeadEquation :: (Show a,Ord a,Floating a) => Grid Element a -> M.Map Int a ->M.Map Int a -> [a]
jacobianNodeHeadEquation grid  vm nh =  term <$> l
  where
    l = links grid
    sflow = signedFlow grid vm
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t conf ) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTee conf (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow grid
    addTee k = maybe 0 id (M.lookup k nodeLosses)
    term (l,h,t,e) =   sum (pipeElement (var l vm) <$> e) - ( varn h nh  + (varr3 h nhs ^. _z) *9.78235  )  +  addTee (h,l) + addTee (t,l) + ( ((varr3 t nhs ^. _z) *9.81 ) + varn t nh )
      where
         nhs = fmap fst (M.fromList $shead grid)

ktubo t  = perda*10/(1000*60)**1.85
        where
              d = case diametroE t of
                       Just d -> d
                       i -> errorWithStackTrace $ "elemento sem diametro " <>  show t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))



jacobianEqNodeHeadGrid l vh = loops <> nodes
    where loops =  jacobianNodeHeadEquation l v h
          nodes =  jacobianContinuity  l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh


