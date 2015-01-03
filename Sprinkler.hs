{-# LANGUAGE TupleSections,NoMonomorphismRestriction #-}
import Control.Monad
import Numeric.GSL.Minimization
import Data.Maybe
import Data.Packed.Matrix(Matrix)
import Data.List as L
import qualified Data.Traversable as Tra
import qualified Data.Map as M
import Control.Applicative
import Debug.Trace

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

areaAplicacao = 140 :: Double
densidadeAgua = 4.1 :: Double
coberturaChuveiro =  espacamento*distancia
espacamento = 3.30
distancia = 3.60
nChuveiros = ceiling $ areaAplicacao/coberturaChuveiro
larguraAplicacao = 1.2* sqrt areaAplicacao
chuveirosRamal = ceiling $ larguraAplicacao / distancia
vazaoMin = 11*densidadeAgua

joelhos = M.fromList
    [((("Conexao","Joelho","90"),130),M.fromList [(32,1.5),(40,3.2),(50,3.4),(65,3.7)])
    ,((("Conexao","Joelho","90"),100),M.fromList [(25,0.8),(32,1.1),(40,1.3),(50,1.7),(65,2.0)])
    ,((("Conexao","Te","Lateral"),130),M.fromList [(32,4.6),(40,7.3),(50,7.6),(65,7.8),(75,8.0)])
    ,((("Conexao","Te","Lateral"),100),M.fromList [(25,1.7),(32,2.3),(40,2.8),(50,3.5),(65,4.3),(75,5.2)])]

justError e Nothing = error e
justError _ (Just i) = i

guardDiametro dtubom vazao = guard ( (diametroSeg vazao*0.90) < 25e-3  && dtubom == 25e-3 || dtubom> diametroSeg vazao *0.90  && dtubom < diametroSeg vazao *1.10)



unfoldE (RamalElement l) (p:v:xs,Node pinit vinit _ ) =  do
  last@((xlast,(Node plast vlast _)):_)  <- foldr (\i j ->  concat $ mapM (\l -> fmap (:l) $ unfoldE i  (head l) ) j ) ([fmap (xs,) [Node p v (last l)]]) (init l)
  guard $ (abs $ plast - pinit ) < 1e-2
  return (xlast ,RamalNode pinit  (vinit + vlast) (fmap snd last)  undefined)
unfoldE i (xs,j) = Tra.traverse (addElement i) (xs,j)

addElement (Joelho _ tipo jd c)  (Node p v (Tubo (Just dtubom) _ _))= do
  let
    vazao = v /1000/60
    a = area dtubom
    distancia =  justError (show ((tipo,c), dtubom*1000)) $join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
    perda =  10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
  guardDiametro dtubom vazao
  return (Node (p + perda*10) v (Joelho (Just dtubom) tipo jd c))

addElement (Tubo _ distancia c) nodo  = do
  dtubo <- diametrosTubos
  let
    v = vNode nodo
    p = pNode nodo
    vazao = v /1000/60
    dtubom =   dtubo/1000
    a = area dtubom
    perda = 10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
  -- Erro de 20% do diametro calculado
  guardDiametro dtubom vazao
  return ( Node (p + perda*10) v (Tubo (Just dtubom) distancia c))


addElement s@(RamalElement l)  (Node pinit vinit o)= do
  initPath @((Node plast vlast _): _)  <- foldr (\i j-> concat $ mapM (\l -> fmap (:l) $ addElement i (head l) ) j ) [[Node  (pressao vazaoMin 5.8) vazaoMin (last l) ]] (init l)

  let
      fun :: [Double] -> Double
      fun (p: v:xs) = if null res then pinit else minimum  res
          -- where res = fmap abs $ fmap (pinit-)  (fmap pressaoNode $ fmap snd $ foldr (\i j->  concat $ fmap  (unfoldE i) j) (fmap (xs,) [Node p v (last l)]) (init l))
          where res = fmap abs $ fmap (pinit-)  (fmap pressaoNode $ foldr (\i j->  concat $ fmap  (addElement i) j) ([Node p v (last l)]) (init l))
      ([pmin,vmin],resPath) = minimize NMSimplex 1e-4 300 (fmap (*0.5) initvalue ) fun initvalue
      initvalue = [pressao vazaoMin 5.8,vazaoMin]
  path@((Node plast vlast _): _)  <- foldr (\i j-> concat $ mapM (\l -> fmap (:l) $ addElement i (head l) ) j ) [[Node  pmin vmin (last l) ]] (init l)
  guard $ (abs $ plast - pinit ) < 1e-2
  return  (RamalNode pinit (vinit +vlast) path  resPath )

addElement s@(Sprinkler _ area densidadeMin)  (Node p v o) = do
  b@(d,k1) <- bicos
  guard $ (k1*sqrt p) /area > densidadeMin
  return $ Node p (v +  k1*sqrt p)  (Sprinkler (Just b) area densidadeMin)
addElement i j = error (show i <> show j)

result = foldr (\i j -> concat $ fmap (\l -> fmap (:l) $ addElement i (head l) ) j ) [[(Node (pressao vazaoMin 8.0) vazaoMin (Sprinkler (Just (13,8)) coberturaChuveiro densidadeAgua))]]

renderNode = putStrLn . L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show l ++ "\n" ++ v) . zip [0..]. fmap (L.intercalate "\n") . fmap (fmap ("\t"++)) . fmap (fmap (showNode 0)) . result

showNode i n@(Node _ _ _) = (concat $ replicate i "\t") ++ show n
showNode i (RamalNode p v n res ) = (concat $ replicate i "\t") ++ "RamalNode " ++ show p ++ "  " ++ show v ++ {-"  " ++ show res ++-} "\n" ++ (L.intercalate "\n" $ fmap (showNode (i +1))  n)

-- pathData = [RamalElement ramal,Joelho Nothing ("Conexao","Te","Lateral") 130 ,  Tubo Nothing 3.3 130 , RamalElement ramal , Tubo Nothing 3.3 130,Joelho Nothing ("Conexao","Joelho","90") 130, Tubo Nothing 12.40 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.6 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130]

-- ramal = [Joelho Nothing ("Conexao","Joelho","90") 130,Tubo Nothing 12.4 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.6 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua, Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua]

initialSprinkler vmin  = do
  b@(d,k) <- bicos
  guard $ (pressao vmin k ) > 50
  return (Node (pressao vmin k) vmin (Sprinkler (Just b) coberturaChuveiro densidadeAgua))


main = renderNode1 mainpath
  where r1 =  RamalElement [tubo 1.78,sp]
        r2 =  RamalElement [tubo 0.40,sp,tubo 2.48,sp,tubo 3.10,joelho DLeft ,tubo 1.1,sp]
        r3 =  RamalElement [tubo 2.16,sp]
        r4 =  RamalElement [tubo 2.65,sp]
        r5 =  RamalElement [tubo 0.6,sp]
        path = [te DLeft ,tubo 0.3,r3,te DRight ,tubo 2.00,r2 ,te DLeft,tubo 1.02,r4 ,joelho DRight ,tubo 0.59 ,te DLeft,tubo 0.48 ,r1 , tubo 5.25 ,joelho DLeft , tubo 1.78 ]
        mainpath = [r5 , te DLeft ,tubo 1.54,RamalElement (path ++ [sp]), tubo 1.40 ] ++ path
        sp = Sprinkler Nothing coberturaChuveiro densidadeAgua
        tubo d = Tubo Nothing d 100
        joelho d = Joelho Nothing ("Conexao","Joelho","90") d 100
        te d = Joelho Nothing ("Conexao","Te","Lateral") d 100
        result1 = scanl (\j i-> concat $ fmap (\l-> fmap (:l) $ addElement i (head l)) j) ((\i -> [i]) <$> initialSprinkler vazaoMin) . reverse
        renderNode1 = mapM (\(i,l) -> writeFile ("res"<> show i) . (("\n ----------------- \n Iteration - " ++ show i ++ "\n ----------------- \n" ) ++ ).  L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show (l :: Int) ++ "\n" ++ v) . zip [0..]. fmap (L.intercalate "\n") . fmap (fmap (showNode 0)) $ l)  . zip [0..].  result1

data Direction
  = DLeft
  | DRight
  deriving(Show)

data TeTipo
  = TeLateral
  | TeFrontal
  deriving(Show)

data Element
  = Tubo
  { diametro :: Maybe Double
  , comprimento :: Double
  , atrito :: Double
  }
  |Joelho
  { diametroJ :: Maybe Double
  , tipoJ :: (String,String,String)
  , direction :: Direction
  , material :: Double
  }
  | Sprinkler
  { tipo :: Maybe (Double,Double)
  , areaCobertura :: Double
  , densidadeMin :: Double
  }
  | Te
  { tipoTe ::  TeTipo
  , pathRight :: [Element]
  , pathLeft :: [Element]
  }
  | RamalElement
  { pathElement :: [Element]
  }deriving(Show)

data Node
  = Node
  { pressaoNode :: Double
  , vazao :: Double
  , elemento :: Element
  }
  | RamalNode
  { pressaoRamal :: Double
  , vazaoRamal :: Double
  ,pathNode :: [Node]
  , resPath :: Matrix Double
  }deriving(Show)

pNode (Node p _ _) = p
pNode (RamalNode p _ _ _ ) = p
vNode (Node _ v _ ) = v
vNode (RamalNode _ v _ _ ) = v



periodoFuncionamento = 1/24
area d = pi*(d/2)**2
diametrosTubos = [25,32,40,50,65,75,100]
bicos = [(11,5.8){-,(10,3.7),(11,5.8),(13,8),(14,11.5),(16,15.1)-}]
diametroSeg qseg = 1.3*sqrt qseg *periodoFuncionamento**0.25
pressao q k = (q/k)**2


data Name a = Name a Element

sprinkler (Name n (Sprinkler (Just (i,j))  _ _) )= circle (10*i/1000.0) #  fc red # named n # lw none
segment (Name n (Tubo (Just d) c _)) =fromSegments [ straight (r2 (0, c))] # lw  (Output d) # lc red


