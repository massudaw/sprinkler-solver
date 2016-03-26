{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,GeneralizedNewtypeDeriving,FlexibleContexts,TypeFamilies,DeriveFunctor,DeriveFoldable,DeriveTraversable#-}
{-# LANGUAGE TupleSections,DeriveFunctor,DeriveFoldable #-}
module Element where

import Hydraulic
import qualified Position as P
import Control.Lens ((^.))
import Debug.Trace
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
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as F

import Linear.V3
import Rotation.SO3 hiding (rotM)



instance PreSys Element  where
  type NodeDomain Element = Identity
  type LinkDomain Element= Identity
  revElem (Joelho i j b k) = Joelho i j (-b) k
  revElem (Turn i ) = Turn i
  revElem i = i
  constrained (Reservatorio i) = Identity (Just 0)
  constrained i = Identity $ Nothing
  lconstrained i = Identity $ Nothing

instance Coord Element (V3 Double) where
  nextElement  = nextS
  thisElement l i = (2,). (\j-> (0,SO3 $ P.rotM $ fmap opi $ (V3 0 0 j))) <$> this  (F.toList l,i)
    where
      this e  =  (M.fromList ( els $ first F.toList  e))
        where
          els (_,(Tee (TeeConfig [rl,br,bl,rr] _ ang  _ _ _) _ ))
            =  [(rl,1/2 - t ),(rr,-t),(br,0),(bl,1/2)]
              where t = ang/pi/2
          els (_,(Tee (TeeConfig [rl,b,rr] _ ang  _ _ _) _ ))
            =  [(rl,1/2 - t ),(rr,-t),(b,0)]
              where t = ang/pi/2
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
      lengthE (OpenTubo _ c _ ) = r3 (c,0,0)
      lengthE i = 0
      r3 (x,y,z) = V3 x y z



pipeElement v e | v < 0 = negate $ pipeElement (abs v) e
pipeElement v (Bomba  ((pn,vn)) (Poly l ) ) = negate $ (*pn) $ (/100)  $foldr1 (+) (polyTerm <$> l)
      where polyTerm (0,c) =   c
            polyTerm (p,c) =   c*(100*v/vn)**p
-- pipeElement v e@(Resistive k p)  = k*v**p
pipeElement v e@(Tubo _ _ _)  = (ktubo e)*v**1.85
pipeElement v e@(OpenTubo _ _ _)  = (ktubo e)*v**2
pipeElement v e@(Joelho _ _ _ _)  = (ktubo e)*v**1.85
pipeElement v e@(Perda __ _ _)  = (ktubo e)*v**1.85
pipeElement v (Turn _)   = 0


signedFlow :: (Show a,Floating a )=> Grid Element a -> M.Map Int a ->M.Map Int (M.Map Int a)
signedFlow g v = M.fromList $  fmap (\(i,_) ->  (i,) $ M.fromList $ ( ( sumn $ flipped i $ links g) ++   ((suma $ correct i $ links g))) ) (nodesFlow g)
  where flipped i=  filter (\(_,(h,t,_)) -> h == i )
        correct i= filter (\(_,(h,t,_)) -> t == i )
        suma =  fmap (\(li,_) -> (li,var li v ))
        sumn =  fmap (\(li,_) ->  (li,negate $ var li v))

jacobianContinuity :: (Show a,Ord a,Floating a )=> Grid Element a -> M.Map Int a -> M.Map Int a -> [a]
jacobianContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $ filter (not . isReservoir . snd) $ nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,(h,t,_)) -> h == i )
        correct i= suma . filter (\(_,(h,t,_)) -> t == i )
        suma =  fmap (\(li,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        nflow i e = genFlow (var i pm) e
        genFlow _ (Open i ) = i
        genFlow _ (Tee _ _ ) = 0
        genFlow idf (Sprinkler (Just (_,k)) _ _ _) = k*sqrt(abs idf)
        genFlow _ (Reservatorio _  ) = errorWithStackTrace "reservatorio doesn't preserve flow"

varn h = justError " no press" .  M.lookup h
varr3 h = justError "no el " .  M.lookup h

-- Generic Solver | Node + Head Method
jacobianNodeHeadEquation :: (Show a,Ord a,Floating a) => Grid Element a -> M.Map Int a ->M.Map Int a -> [a]
jacobianNodeHeadEquation grid  vm nh =  term <$> l
  where
    l = links grid
    sflow = signedFlow grid vm
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t conf ) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTee conf (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow grid
    addTee k = maybe 0 id (M.lookup k nodeLosses)
    term (l,(h,t,e)) =   (sum (pipeElement (var l vm) <$> e) ) + (varr3 t nhs ^. _z - varr3 h nhs ^. _z)*9.78235  + ( varn t nh - varn h nh )  +  addTee (h,l) + addTee (t,l)
      where
         nhs = fmap fst (M.fromList $shead grid)

manning t  = perda*10/(1000*60)**1.85
        where
              d = case diametroE t of
                       Just d -> d
                       i -> errorWithStackTrace $ "elemento sem diametro " <>  show t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 4.66*(distanciaE t)*c/(d**(16/3))


ktubo t  = perda*10/(1000*60)**1.85
        where
              d = case diametroE t of
                       Just d -> d
                       i -> errorWithStackTrace $ "elemento sem diametro " <>  show t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))

jacobianEqNodeHeadGrid :: (Show a , Ord a ,Floating a) => Grid Element a -> M.Map Int (LinkDomain Element a) -> M.Map Int (LinkDomain Element  a) -> [a]
jacobianEqNodeHeadGrid = (\l v h -> jacobianNodeHeadEquation l (runIdentity <$> v) (runIdentity <$> h) <> jacobianContinuity l (runIdentity <$> v) (runIdentity <$> h))

rel = [tubod dm 0.5 , Turn (1/4) ,joelhoR  , tubod dm 1.2 , joelhoL,Turn (-1/4) , tubod dm 0.5]
  where
      dm = 0.08
      tubod  d l  = Tubo (Just d) l 100
      joelhoR  = Joelho Nothing ("Conexao","Joelho","90") right90  100
      joelho  = joelhoR
      joelhoL  = Joelho Nothing ("Conexao","Joelho","90") left90  100
      rightC d = d
      right90  = rightC $ 1/4
      leftC d = -d
      left90  = leftC $ 1/4

rori = (V3 1 0 (0::Double) , SO3 $rotM (V3 0 0 (-pi/2) :: V3 Double))


