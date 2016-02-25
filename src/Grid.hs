{-# LANGUAGE DeriveFunctor,NoMonomorphismRestriction,TypeFamilies,TupleSections ,RankNTypes #-}
module Grid where
import Data.Maybe
import Control.Applicative
import Control.Monad
import Eletric
import Thermal
import Rotation.SO3
import Data.Monoid
import Tee hiding (ktubo)
import Element
import Numeric.GSL.Root
import qualified Data.Map as M

import Numeric.AD
import Control.Lens
import Linear.V3


data Orientation
 = Clock
 | CounterClock
 deriving(Show)

data Grid b a
  = Grid
  { linksPosition :: [(Int,[(V3 a ,SO3 a  )])]
  , links :: [(Int,Int,Int,[b a])]
  , shead :: [(Int,(V3 a,SO3 a ))]
  , nodesFlow :: [(Int,b a)]
  }deriving(Functor,Show)



data Iteration b a
  = Iteration
  { flows :: [(Int,a)]
  , pressures :: [(Int,a)]
  , grid :: Grid b a
  }deriving(Show,Functor)


isTee (Tee _ _ ) = True
isTee i = False

ktubo t  = perda*10/(1000*60)**1.85
        where
              d = case diametroE t of
                       Just d -> d
                       i -> error $ "elemento sem diametro " <>  show t
              c = materialE t
              -- note : abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(distanciaE t)/((c**1.85)*(d**4.87))

thermalEq l vh = loops <> nodes
    where loops =  thermalPotential l v h
          nodes =  thermalContinuity l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh



circuitEq l vh = loops <> nodes
    where loops =  circuitPotential l v h
          nodes =  circuitContinuity l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh


jacobianEqNodeHeadGrid l vh = loops <> nodes
    where loops =  jacobianNodeHeadEquation l v h
          nodes =  jacobianContinuity  l v h
          nlinks =length (links  l)
          v = M.fromList $ zip (fmap (\(i,_,_,_) -> i) $ links l)  $ take nlinks vh
          h = M.fromList $ zip ( fmap fst $ nodesFlow l) $ drop nlinks vh

printResidual iter@(Iteration n f a) modeler = modeler a  (snd <$> flows iter <> pressures iter )

solveIter :: forall c . (forall  a . (Show a , Ord a , Floating a ) => Iteration  c a ) -> (forall   b. (Show b, Ord b, Floating b) => Grid c b -> [b] -> [b] ) -> Iteration c Double
solveIter iter modeler =  Iteration (zip (fmap (\(i,_,_,_) -> i) $ links $ grid iter) $ take fl res) (zip (fmap fst $ nodesFlow $ grid iter)  $ drop fl res) (grid iter)
  where
    fl = length (flows iter)
    res = fst . rootJ HybridsJ 1e-7 1000 (modeler (grid iter) ) (jacobian (modeler (grid iter)  ) )  $ (snd <$> flows iter <> pressures iter )

var :: Show a => Int -> M.Map Int a -> a
var i m = case M.lookup i m of
               Just i -> i
               Nothing -> error $ "no variable " ++ show i  ++ " " ++ show m

thermalElement v (Conductor i ) = v*i
thermalElement _ (HeatFlow i) = i

circuitElement v (Resistor i ) = v*i
circuitElement _ (VoltageSource i ) = i



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

-- Generic Solver | Node + Head Method
thermalPotential :: (Show a,Ord a,Floating a) => Grid Thermal a -> M.Map Int a ->M.Map Int a -> [a]
thermalPotential grid  vm nh =  term <$> l
  where
    l = links grid
    term (l,h,t,e) =   sum (thermalElement (var l vm) <$> e) - lookNode h   +   lookNode t
    lookNode h = justError "cant find node "  $ (join $ fmap (\i -> if isAmbient i then (\(Ambient i) -> Just i) $ i else Nothing) $ varM h (M.fromList $ nodesFlow grid)) <|> varM h nh
      where
        varM h = M.lookup h


isAmbient (Ambient i) = True
isAmbient i = False

thermalContinuity :: (Show a,Ord a,Floating a )=> Grid Thermal a -> M.Map Int a -> M.Map Int a -> [a]
thermalContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $ filter (not . isAmbient . snd) $ nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,h,t,_) -> h == i )
        correct i= suma . filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        genFlow _ ThermalNode  = 0
        nflow i e = genFlow (var i pm) e


-- Generic Solver | Node + Head Method
circuitPotential :: (Show a,Ord a,Floating a) => Grid Eletric a -> M.Map Int a ->M.Map Int a -> [a]
circuitPotential grid  vm nh =  term <$> l
  where
    l = links grid
    term (l,h,t,e) =   sum (circuitElement (var l vm) <$> e) - lookNode h   +  lookNode t
    lookNode h = justError "cant find node " $ (join $ fmap (\i -> if isGround i then (\(Ground ) -> Just 0) $ i else Nothing) $ varM h (M.fromList $ nodesFlow grid)) <|> varM h nh
      where
        varM h = M.lookup h


isGround Ground = True
isGround i = False

circuitContinuity :: (Show a,Ord a,Floating a )=> Grid Eletric a -> M.Map Int a -> M.Map Int a -> [a]
circuitContinuity g v pm = fmap (\(i,e) -> sum (flipped i $ links g) +  (sum ( correct i $ links g))  - nflow i e) $ filter (not . isGround . snd) $ nodesFlow g
  where
        -- pipeFlow
        flipped i=  sumn . filter (\(_,h,t,_) -> h == i )
        correct i= suma . filter (\(_,h,t,_) -> t == i )
        suma =  fmap (\(li,_,_,_) -> var li v )
        sumn =  fmap negate . suma
        -- nodeFlow
        genFlow _ Node  = 0
        genFlow _ Ground = 0
        nflow i e = genFlow (var i pm) e


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
        genFlow _ (Reservatorio _  ) = error "reservatorio doesn't preserve flow"

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

-- Rendering System Equations
printMatrix :: Show a => [a] -> IO ()
printMatrix  = putStr . unlines . fmap show

