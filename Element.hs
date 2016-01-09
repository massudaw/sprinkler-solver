{-# LANGUAGE DeriveFunctor,DeriveFoldable #-}
module Element where
import Data.Foldable
import qualified Data.Map as M
import Control.Monad

import Linear.V3

data Direction a
  = Direction a a
  deriving(Eq,Show,Functor)


data TeTipo
  = TeBranch
  | TeRunL
  | TeRunR
  deriving(Eq,Show)

data Curva a = Curva (a -> a)
           | Poly [(a,a)]

instance Functor Curva  where
  -- fmap f (Curva l )  = Curva $ fmap f  l
  fmap f (Poly l ) = Poly $ fmap (\(i,j) -> (f i,f j)) l

instance Eq (Curva  a) where
  _ == _ = True
instance Show (Curva  a) where
  show _ = ""

data Element a
  = Tubo
  { diametro :: Maybe a
  , comprimento :: a
  , atrito :: a
  }
  | Gravidade
  { distanciaQueda :: a
  }
  | Reservatorio
  { tempo :: a
  , volume :: Maybe a
  , element :: Element a
  }
  | Tip
  | Open
  { openFlow :: a
  }
  | Resistive
  {  load :: a
  ,  power :: a
  }
  | DiameterChange
  { input :: Maybe a
  , output :: Maybe a
  , material :: a
  }
  | Bocal
  { diametroJ :: Maybe a}
  | Bomba
  { nominalValues :: Maybe (a,a)
  , curva :: Curva a
  , recalqueElements :: [Element a]
  , succaoElements :: [Element a]
  }
  | Tee (TeeConfig a)
  | Joelho
  { diametroJ :: Maybe a
  , tipoJ :: (String,String,String)
  , direction :: (a,a)
  , material :: a
  }
  | Perda
  { diametroJ :: Maybe a
  , tipoJ :: (String,String,String)
  , material :: a
  }
  | Sprinkler
  { tipo :: Maybe (a,a)
  , diametroJ :: Maybe a
  , areaCobertura :: a
  , densidadeMin ::a
  }
  | Te
  { diametro :: Maybe a
  , tipoTe ::  TeTipo
  , pathRight :: [Element a]
  , pathLeft :: [Element a]
  }
  | OptTe
  { tipoTe ::  TeTipo
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
  | OptionalPath
  { pathOption :: [Element a]
  }
  | RamalElement
  { pathElement :: [Element a]
  }deriving(Eq,Show,Functor)

data Node a
  = Node
  { pressaoNode :: Double
  , vazao :: Double
  , elemento :: a
  }
  | RamalNode
  { pressaoNode :: Double
  , vazao :: Double
  , pathNode :: [Node a]
  }
  | OptionalNodes
  { pressaoNode :: Double
  , vazao :: Double
  ,pathNodes :: [[Node a]]
  }
  | OptionalNode
  { pressaoNode :: Double
  , vazao :: Double
  ,pathNode :: [Node a]
  }deriving(Eq,Foldable,Show)

data TeeConfig a
  = TeeConfig
  { teeConfig :: [Int] -- Left Run Branch Right Run
  , teeRadius :: a
  , teeDiameterBranch :: a
  , teeDiameterRun :: a
  , teeMaterial :: a
  }
  | StaticTee
  { teeConfig :: [Int] -- Left Run Branch Right Run
  , teeRadius :: a
  , teeDiameterBranch :: a
  , teeDiameterRun :: a
  , teeMaterial :: a
  }
  deriving(Eq,Ord,Show,Functor)

diametroH ,diametroT:: Element a -> Maybe a
diametroH (Reduction h _ ) = Just h
diametroH i = diametroE i

diametroT (Reduction _ t ) = Just t
diametroT i = diametroE i

diametroE :: Element a -> Maybe a
diametroE (Tubo d _ _ ) = d
diametroE (Joelho d _ _ _) = d
diametroE i = Nothing

distanciaE :: (Show a,Ord a,Fractional a )=> Element a -> a
distanciaE (Tubo _ d _ ) = d
distanciaE (Joelho (Just dtubom) tipo _ c) =  justError (show (tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos

materialE :: Show a => Element  a -> a
materialE (Tubo _ _ c) =  c
materialE (Joelho _ _ _ c) = c
materialE i = error $ "No Material Defined" ++ show i

elementE (Node _ _ e) = Just e
elementE i = Nothing


joelhos :: (Ord a ,Fractional a,Num a )=> M.Map ((String,String,String),a) (M.Map a a)
joelhos = M.fromList
    [((("Conexao","Joelho","90"),130),M.fromList [(32,1.5),(40,3.2),(50,3.4),(65,3.7)])
    ,((("Conexao","Joelho","90"),100),M.fromList [(25,0.8),(32,1.1),(40,1.3),(50,1.7),(65,2.0),(75,2.5),(80,2.5),(100,3.4),(125,4.2),(150,4.9),(200,6.4)])
    ,((("Valvula","","Gaveta"),100),M.fromList [(25,0.2),(32,0.2),(40,0.3),(50,0.4),(65,0.4),(75,0.5),(80,0.5),(100,0.7),(125,0.9),(150,1.1)])
    ,((("Bocais","Saida",""),100),M.fromList [(25,0.7),(32,0.9),(40,1.0),(50,1.5),(65,1.9),(75,2.2),(80,2.2),(100,3.2),(125,4.0),(150,5.0)])
    ,((("Valvula","Retencao",""),100),M.fromList [(25,2.1),(32,2.7),(40,3.2),(50,4.2),(65,5.2),(75,6.3),(80,6.3),(100,8.4),(125,10.4),(150,12.5)])
    ,((("Conexao","Joelho","45"),100),M.fromList [(25,0.4),(32,0.5),(40,0.6),(50,0.8),(65,0.9),(75,1.2),(80,1.2),(100,1.5),(125,1.9),(150,2.3),(200,3),(250,3.8)])
    ,((("Conexao","Te","Lateral"),130),M.fromList [(32,4.6),(40,7.3),(50,7.6),(65,7.8),(75,8.0)])
    ,((("Conexao","Te","Direta"),100),M.fromList [(25,0.5),(32,0.7),(40,0.9),(50,1.1),(65,1.3),(75,1.6),(80,1.6),(100,2.1),(125,2.7),(150,3.4)])
    ,((("Conexao","Te","Lateral"),100),M.fromList [(25,1.7),(32,2.3),(40,2.8),(50,3.5),(65,4.3),(75,5.2),(80,5.2),(100,6.7),(125,8.4),(150,10.0)])]

data Grid a
  = Grid
  { linksPosition :: [(Int,[(V3 Double,(a,a,a))])]
  , links :: [(Int,Int,Int,[Element a])]
  , shead :: [(Int,(V3 Double,(a,a,a)))]
  , nodesFlow :: [(Int,Element a)]
--  , origin :: (Int,R3 Double,(a,a,a))
  }deriving(Show,Functor)


isReservoir (Reservatorio _ _ _) = True
isReservoir _ = False

justError e Nothing = error e
justError _ (Just i) = i
