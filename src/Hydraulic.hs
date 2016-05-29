{-# LANGUAGE TupleSections,DeriveFunctor,DeriveFoldable #-}
module Hydraulic where

import Position hiding (rotM)
import TBL.Parser
import qualified Position as P
import Linear.V2
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
isGrelha (Grelha  _ _ _ _ ) = True
isGrelha _ = False
isTee (Tee _  _ ) = True
isTee i  = False
isReservoir (Reservatorio _ ) = True
isReservoir _ = False
isEntry (Tee (TeeConfig _ _ (RectangularEntry _ _ _)) _ ) = True
isEntry (Tee (TeeConfig _ _ (RoundEntry  _ _)) _ ) = True
isEntry (Tee (TeeConfig _ _ (DuctEntry _ _)) _ ) = True
isEntry _ = False

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

data ElbowConstruction
  = Mitted
  | Pleated
  | Gored Int
  | DieStamped
  | FlatBack
  deriving(Show,Eq,Ord)

coverageArea (SPKCoverage x y _ _ ) = x*y


data FanMode a
  = Centrifugal
  deriving(Show,Eq,Ord,Functor)

data Section  a
  = Circular a
  | Rectangular a a
  deriving(Eq,Ord,Show,Functor)

data InternalConfig a
  = RoundTee
  { teeAngle :: a
  , teeRadius :: a
  , teeMaterial :: a
  }
  | Diffusor
  { interactionElement :: InternalConfig a
  , fanMode :: FanMode a
  }
  | Transition
  { transitionAngle :: a }
  | RectangularTee
  { teeMaterial :: a
  }
  | FanSystemInteraction
  { interactionElement :: InternalConfig a
  , fanlength :: a
  , fanMode :: FanMode a
  }
  | Elbow
  { angleElbow :: a
  , radiusElbow :: a
  , construction :: ElbowConstruction
  }
  | Screen
  { nscreen :: a
  , areascreen :: a
  }
  | FireDamper
  | Damper
  { angle :: a
  , diameterDamper :: a
  }
  | RoundEntry
  { radius:: a
  , wall :: Bool
  }
  | DuctEntry
  { wallLength :: a
  , tubeThickness:: a
  }
  | RectangularEntry
  { angle  :: a
  , lengthEntry ::  a
  , wall :: Bool
  }
  deriving(Eq,Ord,Show,Functor)

data TeeConfig a
  = TeeConfig
  { teeConfig :: [Int]
  , teeSection :: [Section a]
  , teeInternal :: InternalConfig a
  }
  deriving(Eq,Ord,Show,Functor)

data TeeConfigType a
  = Table
  | Formula
  | TBL
  deriving (Eq,Ord,Show,Functor)

data TabelaPerda a
  = TabelaPerda
  { section :: Section a
  , tipoJ :: (String,String,String)
  , materialP :: a
  }deriving(Eq,Show,Ord,Functor)

data Element a
  -- Tubo Fechado
  = Tubo
  { secao :: Section a
  , comprimento :: a
  , atrito :: a
  }
  | Reservatorio
  { tempo :: a
  }
  | Open
  { openFlow :: a
  }
  | Bomba
  { nominalValues :: (a,a)
  , curva :: Curva a
  }
  | Tee (TeeConfig a) (TeeConfigType a)
  | Joelho
  { direction :: a
  , perdas :: TabelaPerda a
  }
  | Turn
  { jointTurn :: a
  }
  | Perda
  { perdas :: TabelaPerda a
  }
  | Sprinkler
  { tipo :: (a,a)
  , diametroJ :: a
  , areaCobertura :: SPKCoverage a
  , densidadeMin ::a
  }
  | Grelha
  { height ::a
  , width:: a
  , areaGrelha :: a
  , velocidadeMin ::a
  }
  deriving(Eq,Show,Functor)

data Tree a
  = Te
  { tediametro :: a
  , tipoTe ::  TeTipo
  , pathRight :: [Tree a]
  , pathLeft :: [Tree a]
  }
  | Atom (Element a)
  | Origem
  { elements :: [Tree a]
  }

data Ambient a
  = Ambient
  { fluido :: Fluido a
  , ashrae :: M.Map String [(String,TableType a )]
  , altitude :: a
  , temperaturaAmbiente :: a
  , pontoOrvalho :: a
  , pressaoBarometrica :: a
  , geoposition :: V2 a
  }deriving(Eq,Ord,Functor,Show)


data Fluido a
  = Fluido
  { viscosity :: [((a,a),a)]
  , density :: [((a,a),a)]
  , fluidName :: String
  }deriving(Eq,Ord,Show,Functor)


kinematicViscosity p t f = justError "no interp" $ (/) <$> bilinearInterp (p,t) (M.fromList $ viscosity f) <*> bilinearInterp (p,t) (M.fromList $ density  f)

hydraulicDiameter (Rectangular w h) = 1.30*(w*h)**0.625/(w+h)**0.25
hydraulicDiameter (Circular d ) = d

areaS :: Floating a => Section a -> a
areaS (Rectangular w h ) = w*h
areaS (Circular d) = pi*(d/2)^2

areaE :: (Show a,Floating a) => Element a -> a
areaE (Tubo (Rectangular i j) _ _ ) = i*j
areaE e = pi*(( diametroE e)/2)^2

perimeterE (Tubo (Rectangular i j) _ _) =  2*i+2*j
roughnessE e = justError "no roughness" $ M.lookup (materialE e) (M.fromList [(100,0.045),(120,0.09)])

sectionE  (Tubo d _ _ ) = d
sectionE  i = errorWithStackTrace (show i)

diametroE :: (Show a, Floating a )=> Element a -> a
diametroE (Tubo d _ _ ) = hydraulicDiameter d
diametroE (Joelho _ p ) = hydraulicDiameter (section p)
diametroE (Perda d ) = hydraulicDiameter (section d)
diametroE i = errorWithStackTrace ("no diameter" <> show i)

distanciaE :: (Show a,Ord a,Floating a )=> Element a -> a
distanciaE (Tubo _ d _ ) = d
distanciaE (Joelho _ (TabelaPerda s tipo  c)) =  justError (show ("joelho",tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
  where dtubom = hydraulicDiameter s
distanciaE (Perda   (TabelaPerda s tipo c)) = justError (show ("perda",tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
  where dtubom = hydraulicDiameter s
distanciaE i = 0

fittingsE joelhos (Joelho _ (TabelaPerda s tipo  c)) =  justError (show ("joelho",tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
  where dtubom = hydraulicDiameter s
fittingsE joelhos (Perda   (TabelaPerda s tipo c)) = justError (show ("perda",tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
  where dtubom = hydraulicDiameter s

materialE :: Show a => Element  a -> a
materialE (Tubo _ _ c) =  c
materialE (Joelho _ c) = materialP c
materialE (Perda  c) = materialP c
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
    ,((("Valvula","Governo",""),100),M.fromList [(80,5),(100 ,5.84),(150 ,7.84),(200 ,8.84),(250 ,9.84)])
    ,((("Conexao","Te","Lateral"),100),M.fromList [(25,1.7),(32,2.3),(40,2.8),(50,3.5),(65,4.3),(75,5.2),(80,5.2),(100,6.7),(125,8.4),(150,10.0),(200,13),(250,16)])]

isValvulaGoverno (Perda (TabelaPerda _ ("Valvula","Governo",_) _)  ) = True
isValvulaGoverno _ = False

bombaSuccaoFrontal, bombaBipartida :: (Num a ,Fractional a )=> a -> a
bombaSF ,bombaJohnson2,ventiladorLinear:: Floating a => Curva a
bombaSF = Poly [(0,151.229),(1,-0.422331),(2,-0.000979227),(3,- 1.194467786948394e-7)]
bombaJohnson = Poly [ (0,3000/31) ,(1,12/31) ,(2,-324/96875)]
bombaJohnson2 = Poly [(0,3250/31),(1,111 /775) , (2,-486 /484375),(3,-2187 /302734375 )]
bombaSuccaoFrontal x = 151.229 - 0.422331*x - 0.000979227*x^2 - 1.194467786948394e-7*x^3
bombaBP p v  = Bomba (p,v) (Poly [(0,120),(1,0.142857),(2,-0.00134921),(3,-7.936507936507936e-6)])
bombaBipartida x = 120 + 0.0142857*x - 0.00134921*x^2 - 7.936507936507936e-6*x^3

ventiladorLinear = Poly [(0,3440/2200*100),(1,-124*10/2200)]

data DadosBomba  a
  = DadosBomba
  { larguraB :: a
  , comprimentoB :: a
  , potenciaB :: a
  , rotorB :: a
  , pressaoB :: a
  , vazaoB :: a
  , rotacaoB :: a
  }

dadosBomba :: M.Map (Double,Double) (DadosBomba  Double)
dadosBomba = M.fromList $ (\e -> ((pressaoB e ,vazaoB e),e))<$> [DadosBomba 32 200 15 201 75 22 3500,DadosBomba 32 200 15 205  80 20 3500 ,DadosBomba 32 200 15 205 80 22 3500, DadosBomba 50 200 40 204 75 80 3500]

