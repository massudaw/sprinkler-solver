{-# LANGUAGE TupleSections,DeriveFunctor,DeriveFoldable #-}
module Hydraulic where

import Position hiding (rotM)
import qualified Position as P
import Control.Lens ((^.))
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


isBomba (Bomba _ _ ) = True
isBomba _ = False
isSprinkler  (Sprinkler _ _ _ _ ) = True
isSprinkler _ = False
isTee (Tee _  _ ) = True
isTee i  = False
isReservoir (Reservatorio _ ) = True
isReservoir _ = False

data TeTipo
  = TeBranch
  | TeRunL
  | TeRunR
  deriving(Eq,Show)

data Curva a = Poly [(a,a)] deriving(Eq,Show,Functor)


data SPKCoverage a = SPKCoverage
  { xdir :: a
  , ydir :: a
  , hdistance :: a
  , goods :: SPKGoods a
  }deriving(Eq,Ord,Show,Functor)

data SPKGoods a = SPKGoods
  { goodsHeight ::  a
  , goodsClass :: Int
  }deriving(Eq,Ord,Show,Functor)

coverageArea (SPKCoverage x y _ _ ) = x*y

data TeeConfig a
  = TeeConfig
  { teeConfig :: [Int] -- Left Run Branch Right Run
  , teeRadius :: a
  , teeAngle :: a
  , teeDiameterBranch :: a
  , teeDiameterRun :: a
  , teeMaterial :: a
  }
  deriving(Eq,Ord,Show,Functor)

data TeeConfigType = Table | Formula deriving (Eq,Ord,Show)


data Element a
  = Tubo
  { diametro :: Maybe a
  , comprimento :: a
  , atrito :: a
  }
  | OpenTubo
  { diametro :: Maybe a
  , comprimento :: a
  , atrito :: a
  }
  | Reservatorio
  { tempo :: a
  }
  | Open
  { openFlow :: a
  }
  | DiameterChange
  { input :: Maybe a
  , output :: Maybe a
  , material :: a
  }
  | Bomba
  { nominalValues :: (a,a)
  , curva :: Curva a
  }
  | Tee (TeeConfig a) TeeConfigType
  | Joelho
  { diametroJ :: Maybe a
  , tipoJ :: (String,String,String)
  , direction :: a
  , material :: a
  }
  | Turn
  { jointTurn :: a
  }
  | Perda
  { diametroJ :: Maybe a
  , tipoJ :: (String,String,String)
  , material :: a
  }
  | Sprinkler
  { tipo :: Maybe (a,a)
  , diametroJ :: Maybe a
  , areaCobertura :: SPKCoverage a
  , densidadeMin ::a
  }
  | Te
  { diametro :: Maybe a
  , tipoTe ::  TeTipo
  , pathRight :: [Element a]
  , pathLeft :: [Element a]
  }

  | Reduction
  { diametroRH :: a
  , diametroRT :: a
  }
  | Origem
  { elements :: [Element a]
  }
  deriving(Eq,Show,Functor)




diametroE :: Element a -> Maybe a
diametroE (Tubo d _ _ ) = d
diametroE (OpenTubo d _ _ ) = d
diametroE (Joelho d _ _ _) = d
diametroE (Perda d _  _) = d
diametroE i = Nothing

distanciaE :: (Show a,Ord a,Fractional a )=> Element a -> a
distanciaE (Tubo _ d _ ) = d
distanciaE (OpenTubo _ d _ ) = d
distanciaE (Joelho (Just dtubom) tipo _ c) =  justError (show ("joelho",tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
distanciaE (Perda   (Just dtubom)  tipo c) = justError (show ("perda",tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
distanciaE i = 0

materialE :: Show a => Element  a -> a
materialE (Tubo _ _ c) =  c
materialE (OpenTubo _ _ c) =  c
materialE (Joelho _ _ _ c) = c
materialE (Perda _  _ c) = c
materialE i = error $ "No Material Defined" ++ show i



joelhos :: (Ord a ,Fractional a,Num a )=> M.Map ((String,String,String),a) (M.Map a a)
joelhos = M.fromList
    [((("Conexao","Joelho","90"),130),M.fromList [(32,1.5),(40,3.2),(50,3.4),(65,3.7)])
    ,((("Conexao","Joelho","90"),100),M.fromList [(25,0.8),(32,1.1),(40,1.3),(50,1.7),(65,2.0),(75,2.5),(80,2.1),(100,3),(125,3.7),(150,4.5),(200,5.5),(250,6.7)])
    ,((("Valvula","","Gaveta"),100),M.fromList [(25,0.2),(32,0.2),(40,0.3),(50,0.4),(65,0.4),(75,0.5),(80,0.5),(100,0.7),(125,0.9),(150,1.1)])
    ,((("Bocais","Saida",""),100),M.fromList [(25,0.7),(32,0.9),(40,1.0),(50,1.5),(65,1.9),(75,2.2),(80,2.2),(100,3.2),(125,4.0),(150,5.0)])
    ,((("Valvula","Retencao",""),100),M.fromList [(25,2.1),(32,2.7),(40,3.2),(50,4.2),(65,5.2),(75,6.3),(80,6.3),(100,8.4),(125,10.4),(150,12.5)])
    ,((("Conexao","Joelho","45"),100),M.fromList [(25,0.4),(32,0.5),(40,0.6),(50,0.8),(65,0.9),(75,1.2),(80,1.2),(100,1.5),(125,1.9),(150,2.3),(200,3),(250,3.8)])
    ,((("Conexao","Te","Lateral"),130),M.fromList [(32,4.6),(40,7.3),(50,7.6),(65,7.8),(75,8.0)])
    ,((("Conexao","Te","Direta"),100),M.fromList [(25,0.5),(32,0.7),(40,0.9),(50,1.1),(65,1.3),(75,1.6),(80,1.6),(100,2.1),(125,2.7),(150,3.4),(200,4.3),(250,5.5)])
    ,((("Conexao","Te","Direta"),100),M.fromList [(25,0.5),(32,0.7),(40,0.9),(50,1.1),(65,1.3),(75,1.6),(80,1.6),(100,2.1),(125,2.7),(150,3.4),(200,4.3),(250,5.5)])
    ,((("Valvula","Governo",""),100),M.fromList [(100 ,5.84),(150 ,7.84),(200 ,8.84),(250 ,9.84)])
    ,((("Conexao","Te","Lateral"),100),M.fromList [(25,1.7),(32,2.3),(40,2.8),(50,3.5),(65,4.3),(75,5.2),(80,5.2),(100,6.7),(125,8.4),(150,10.0),(200,13),(250,16)])]
    --,((("Conexao","Te","Lateral"),1000),M.fromList [(25,1.7),(32,2.3),(40,2.8),(50,3.5),(65,4.3),(75,5.2),(80,5.2),(100,6.7),(125,8.4),(150,10.0),(200,13),(250,16)])]

isValvulaGoverno (Perda _ ("Valvula","Governo",_) _  ) = True
isValvulaGoverno _ = False

bombaSuccaoFrontal, bombaBipartida :: (Num a ,Fractional a )=> a -> a
bombaSF ,bombaJohnson2:: Floating a => Curva a
bombaSF = Poly [(0,151.229),(1,-0.422331),(2,-0.000979227),(3,- 1.194467786948394e-7)]
bombaJohnson = Poly [ (0,3000/31) ,(1,12/31) ,(2,-324/96875)]
bombaJohnson2 = Poly [(0,3250/31),(1,111 /775) , (2,-486 /484375),(3,-2187 /302734375 )]
bombaSuccaoFrontal x = 151.229 - 0.422331*x - 0.000979227*x^2 - 1.194467786948394e-7*x^3
bombaBP p v  = Bomba (p,v) (Poly [(0,120),(1,0.142857),(2,-0.00134921),(3,-7.936507936507936e-6)])
bombaBipartida x = 120 + 0.0142857*x - 0.00134921*x^2 - 7.936507936507936e-6*x^3


