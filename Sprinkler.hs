{-# LANGUAGE DeriveFoldable,TupleSections,NoMonomorphismRestriction #-}
import Control.Monad
import Numeric.GSL.Minimization
import Data.Ord
import Data.Maybe
import qualified Data.Foldable as F
import Data.Foldable ( Foldable)
import Data.Packed.Matrix(Matrix)
import Data.List as L
import qualified Data.Traversable as Tra
import qualified Data.Map as M
import Control.Applicative hiding ((<|>))
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
    ,((("Conexao","Joelho","90"),100),M.fromList [(25,0.8),(32,1.1),(40,1.3),(50,1.7),(65,2.0),(75,2.5),(100,3.4),(125,4.2),(150,4.9)])
    ,((("Conexao","Te","Lateral"),130),M.fromList [(32,4.6),(40,7.3),(50,7.6),(65,7.8),(75,8.0)])
    ,((("Conexao","Te","Lateral"),100),M.fromList [(25,1.7),(32,2.3),(40,2.8),(50,3.5),(65,4.3),(75,5.2),(100,6.7),(125,8.4),(150,10.0)])]

justError e Nothing = error e
justError _ (Just i) = i

guardDiametro dtubom vazao = guard ( dimMin < 25e-3  && dtubom == 25e-3 || dtubom>   dimMin && dtubom < dimMax )
  where dimMin = diametroSeg vazao *0.9
        dimMax = diametroSeg vazao *1.10

{-
unfoldE (RamalElement l) (v:xs,Node pinit vinit _ ) =  do
  last@((xlast,(Node plast vlast _)):_)  <- foldr (\i j ->  concat $ mapM (\l -> fmap (:l) $ unfoldE i  (head l) ) j ) ([fmap (xs,) [Node (pressao v 5.8) v (last l)]]) (init l)
  -- guard $ (abs $ plast - pinit ) < 1e-2
  return (xlast ,RamalNode pinit  (vinit + vlast) (fmap snd last)  )
unfoldE i (xs,j) = Tra.traverse (addElement i) (xs,j)

unfoldN n@(Node  v p _) = []
unfoldN (RamalNode _ _  i )  = foldr (\i j -> j ++ unfoldN i) [last i] (init i)
-}

minPressure =(\i -> if null i then [] else pure . minimumBy (comparing pNode) $ i)
minFlow =(\i -> if null i then [] else pure . minimumBy (comparing vNode) $ i)

traceEmpty v [] =  traceShow v []
traceEmpty _ i = id i

trace2Empty v [] [] =  []
trace2Empty v i  [] =  traceShow v []
trace2Empty _ _ i = id i

-- solveRamalN :: Node Element -> Node Element -> [Node Element]
solveRamalN s@(RamalNode  _ _  e )  n@(Node pinit vinit o) =  traceEmpty (n) $ minFlow $   do
  let
      l :: [Element]
      l =  fmap extractElement e
      sp@(Node _ vlast es) = last e
      sp2 si v =  calcSprinkler v ssp
        where (Node _ _ ssp)= si
      fun2 spi [v] = if null res then pinit else minimum  res
          where res = fmap abs $ fmap (pinit-)  (fmap pressaoNode $ foldr (\i j->  trace2Empty (i, j) j  $ concat $ mapM (addNode i) j) [sp2 spi v] (init e))
      minimizer spi  = minimize NMSimplex 1e-5 300 (fmap (*0.1) initvalue ) (fun2 spi)  initvalue
      initvalue = [vlast]
      ([vmin],res) =  minimizer sp
      spmin =  sp2 sp vmin
  path@((Node plast vlast _): _)  <- traceEmpty (vmin,res) $ foldr (\i j-> concat $ mapM (\l -> fmap (:l)  $ addElement i (head l) ) j) [[spmin]] (init l)
  -- guard $  (abs $ plast - pinit ) < 1e-2
  return  (Node pinit vlast path)

addNode (Node i j e ) n =  addElement e  n

addNode r@(RamalNode _ _  _ ) n@(Node pinit vinit _ ) = do
  (Node pinit vlast res) <- solveRamalN r n
  return (RamalNode pinit (vinit + vlast ) res )

addNode (OptionalNode p v  r ) n@(Node pinit vinit _ ) = do
  (Node pinit vlast res) <- solveRamalN (RamalNode p v r) n
  return (OptionalNode pinit (max vinit  vlast ) res )

solveRamal s@(RamalElement l)  n@(Node pinit vinit o) =  do
  sp <- filter ((pinit >= ) . pressaoNode) $ initialSprinkler vazaoMin
  let
      sp2 si v =  calcSprinkler v ssp
        where (Node _ _ ssp)= si
      fun2 sp (v:xs) = if null res then pinit else minimum  res
          where res = fmap abs $ fmap (pinit-)  (fmap pressaoNode $ foldr (\i j->  concat $ mapM (addElement i) j) [sp2 sp v] (init l))
      minimizer sp  = minimize NMSimplex 1e-5 300 (fmap (*0.5) initvalue ) (fun2 sp)  initvalue
      initvalue = [vazaoMin]
  let spmin = let ([vmin],_) =  minimizer sp
                    in sp2 sp vmin
  path@((Node plast vlast _): _)  <- foldr (\i j-> concat $ mapM (\l -> fmap (:l)  $ addElement i (head l) ) j) [[spmin]] (init l)
  guard $  (abs $ plast - pinit ) < 1e-3
  return  (Node pinit vlast path)



addElement (Joelho preD tipo jd c)  (Node p v (Tubo (Just dtubom) _ _))= minPressure $ do
  let
    vazao = v /1000/60
    a = area dtubom
    distancia =  justError (show (tipo,c, dtubom*1000)) $join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
    perda =  10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
  return (Node (p + perda*10) v (Joelho (Just dtubom) tipo jd c))
addElement (Joelho preD tipo jd c)  nodo = minPressure $ do
    let
        v = vNode nodo
        p = pNode nodo
    case preD of
      Just dtubom -> do
          let
            vazao = v /1000/60
            a = area dtubom
            distancia =  justError (show (tipo,c, dtubom*1000)) $join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
            perda =  10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
          return (Node (p + perda*10) v (Joelho (Just dtubom) tipo jd c))
      Nothing -> do
         dtubo <- diametrosTubos
         let
           vazao = v /1000/60
           dtubom = dtubo/1000.0
           a = area dtubom
           distancia =  justError (show ((tipo,c), dtubom*1000)) $join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos
           perda =  10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
         guardDiametro dtubom vazao
         return (Node (p + perda*10) v (Joelho (Just dtubom) tipo jd c))
addElement (Tubo _ distancia c)  (Node p v (Joelho (Just dtubom) _ _ _))= minPressure $ do
  let
    vazao = v /1000/60
    a = area dtubom
    perda =  10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
  guardDiametro dtubom vazao
  return (Node (p + perda*10) v (Tubo (Just dtubom) distancia c))

addElement (Tubo preD distancia c) nodo  = minPressure $ do
  let
      v = vNode nodo
      p = pNode nodo
  case preD of
    Just dtubom -> do
      let
          vazao = v /1000/60
          a = area dtubom
          perda =  10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
      return (Node (p + perda*10) v (Tubo (Just dtubom) distancia c))
    Nothing -> do
      dtubo <- diametrosTubos
      let
        vazao = v /1000/60
        dtubom =   dtubo/1000
        a = area dtubom
        perda = 10.65*vazao**1.85*distancia/c**1.85/dtubom**4.87 :: Double
      -- Erro de 20% do diametro calculado
      guardDiametro dtubom vazao
      return ( Node (p + perda*10) v (Tubo (Just dtubom) distancia c))

addElement s@(OptionalPath i ) n@(Node pinit vinit o) = minFlow $ do
  nr@(Node pr vr res ) <- solveRamal (RamalElement i) n
  return $ (OptionalNode pinit (max vr vinit) res)

addElement s@(RamalElement l)  n@(Node pinit vinit o)= minFlow $ do
  (Node pinit vlast res) <- solveRamal s n
  return (RamalNode pinit (vinit + vlast ) res )

addElement g@(Gravidade d) (Node p v o) = do
  return (Node (p - d*10 ) v g)

addElement s@(Sprinkler _ area densidadeMin)  (Node p v o) = do
  b@(d,k1) <- bicos
  guard $ (k1*sqrt p) /area > densidadeMin
  return $ Node p (v +  k1*sqrt p)  (Sprinkler (Just b) area densidadeMin)

addElement i j = error (show i <> show j)

result = foldr (\i j -> concat $ fmap (\l -> fmap (:l) $ addElement i (head l) ) j ) [[(Node (pressao vazaoMin 8.0) vazaoMin (Sprinkler (Just (13,8)) coberturaChuveiro densidadeAgua))]]

renderNode = putStrLn . L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show l ++ "\n" ++ v) . zip [0..]. fmap (L.intercalate "\n") . fmap (fmap ("\t"++)) . fmap (fmap (showNode 0)) . result

showNode i n@(Node _ _ _) = (concat $ replicate i "\t") ++ show n
showNode i (RamalNode p v n ) = (concat $ replicate i "\t") ++ "RamalNode " ++ show p ++ "  " ++ show v ++ {-"  " ++ show res ++-} "\n" ++ (L.intercalate "\n" $ fmap (showNode (i +1))  n)
showNode i (OptionalNode p v n ) = (concat $ replicate i "\t") ++ "OptionalNode" ++ show p ++ "  " ++ show v ++ {-"  " ++ show res ++-} "\n" ++ (L.intercalate "\n" $ fmap (showNode (i +1))  n)

-- pathData = [RamalElement ramal,Joelho Nothing ("Conexao","Te","Lateral") 130 ,  Tubo Nothing 3.3 130 , RamalElement ramal , Tubo Nothing 3.3 130,Joelho Nothing ("Conexao","Joelho","90") 130, Tubo Nothing 12.40 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.6 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130]

-- ramal = [Joelho Nothing ("Conexao","Joelho","90") 130,Tubo Nothing 12.4 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.6 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua, Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua]

calcSprinkler vmin e@(Sprinkler (Just (d,k)) _ _) = (Node (pressao vmin k) vmin e)

initialSprinkler vmin  = do
  b@(d,k) <- bicos
  guard $ (pressao vmin k ) > 50
  guard $ vmin >= 11*densidadeAgua
  return (Node (pressao vmin k) vmin (Sprinkler (Just b) 11 densidadeAgua))

initialSprinkler2 vmin  = minimumBy (comparing pressaoNode ) $do
  b@(d,k) <- bicos
  guard $ (pressao vmin k ) > 50
  guard $ vmin >= 11*densidadeAgua
  return (Node (pressao vmin k) vmin (Sprinkler (Just b) 11 densidadeAgua))
getMinElement s@(Sprinkler (Just (d,k)) a densidadeAgua) = Node (pressao vazaoMin k) vazaoMin s
  where vazaoMin = a * densidadeAgua

{-
test = do
   let    header = "indice;pressao;vazao;elemento;diametro;comprimento;velocidade\n"
          r0 = [tubo 5.25 ,joelho DLeft , tubo 1.78 ]
          r1 =  RamalElement [tubo 1.78,sp]
          r2 =  RamalElement [tubo 0.40,sp,tubo 2.48,sp,tubo 3.10,joelho DLeft ,tubo 1.1,sp]
          r3 =  RamalElement [tubo 2.16,sp]
          r4 =  RamalElement [tubo 2.65,sp]
          r5 =  RamalElement [tubo 0.6,sp]
          path = [tubo 0.3,te DLeft ,r3,tubo 2.00,te DRight ,r2 ,tubo 1.02,te DLeft,r4 ,tubo 0.59 ,joelho DRight ,tubo 0.48 ,te DLeft,r1 ] ++ r0
          mainpath = [r5 , tubo 1.54,te DLeft ,RamalElement (path ++ [sp]), tubo 1.40 ] ++ path
          sp = Sprinkler (Just (11,5.8)) 11 densidadeAgua
          tubo d = Tubo Nothing d 100
          joelho d = Joelho Nothing ("Conexao","Joelho","90") d 100
          te d = Joelho Nothing ("Conexao","Te","Lateral") d 100
          result1 = scanl (\j i-> concat $ fmap (\l-> fmap (:l) $ addElement i (head l)) j) (pure $ pure $ initialSprinkler2 vazaoMin) . reverse
          renderNode1 = mapM (\(i,l) -> when (not $ null l ) $  writeFile ("res"<> backed i) . (("\n ----------------- \n Iteration - " ++ show i ++ "\n ----------------- \n" ) ++ ).  L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show (l :: Int) ++ "\n" ++ v) . zip [0..]. fmap (fmap (\i -> if i == '.' then ',' else i ) . L.intercalate "\n") . fmap (fmap (showNode 0)) $ l)  . zip [0..].  result1
   --mapM (\(i,l) -> writeFile ("opt" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$ last $ result1 mainpath
   -- renderNode1 mainpath
   print ((\i -> show ((pathLength i,pathLoss i) ,i)) mainpath)
-}
openPath (Origem e) = do
  xs <- Tra.traverse openPath e
  return $ concat xs
openPath (Te _ i j) =  path j i ++ path i j
  where path i j = (\i -> if null i then [] else [maximumBy (comparing pathLength) i] )  $ do
          is <- Tra.traverse  openPath i
          js <- Tra.traverse  openPath j
          return (te DRight : RamalElement (concat js) : concat is)
        te d = Joelho Nothing ("Conexao","Te","Lateral") d 100
openPath (OptTe _ i j) =  path j i ++  path i j
  where path i j = (\i -> if null i then [] else [maximumBy (comparing pathLength) i] )  $ do
          is <- Tra.traverse  openPath i
          js <- Tra.traverse  openPath j
          return ( OptionalPath (concat js) : concat is)
        te d = Joelho Nothing ("Conexao","Te","Lateral") d 100

openPath i = return [i]


pathLoss :: [Element] -> [Int]
pathLoss p = let conc =  concat (fmap isRamal p)
             in  sum (fmap isSprinkler p ) : conc
      where isRamal (RamalElement i ) = let conc = concat $ fmap isRamal i
                                            conc2 = fmap isSprinkler i
                                          in sum conc2 : conc
            isRamal (OptionalPath i ) = let conc = concat $ fmap isRamal i
                                            conc2 = fmap isSprinkler i
                                          in sum conc2 : conc
            isRamal i = []
            isSprinkler (Sprinkler _ _ _) = 1
            isSprinkler i = 0

pathLength :: [Element] -> [Int]
pathLength p = (length p) : (concat (fmap isRamal p))
      where isRamal (RamalElement i ) = (length i) : (concat $ fmap isRamal i)
            isRamal (OptionalPath i ) = (length i) : (concat $ fmap isRamal i)
            isRamal i = []
twopass = do
   let    header = "indice;pressao;vazao;elemento;diametro;comprimento;velocidade\n"
          r0 = [tubo 0.25,tubo 0.50,tubo 0.50,tubo 1.0,tubo 1.0 ,tubo 1.0 ,tubo 1.0,joelho DLeft , tubo 1.78 ,sp]
          r1 = [tubo 1.78,sp]
          r2 = [tubo 0.40,sp,tubo 2.48,sp,tubo 3.10,joelho DLeft ,tubo 1.1,sp]
          r3 = [tubo 2.16,sp]
          r4 = [tubo 2.65,sp]
          r5 = [tubo 0.6,sp]
          r7 = [tubo 1.2,sp,tubo 2.8,sp,tubo 1.7,sp]
          r6 = [tubo 0.3,te [tubo 2.0, te [tubo 1.02,te [tubo 0.59,joelho DRight , tubo 0.48 ,te r1 r0] r4] r2] r3]
          l01 = [Gravidade 26.0 ,tubo 26.0  ]  ++ l0 --  ([Gravidade 2.89,tubo 2.89] ++ l0 )
          ssr = [tubo 1.76,sp,tubo 3.57,sp,tubo 3.57,sp,tubo 3.57,sp]
          ss21 = [tubo 1.76  ,joelho DRight] ++ ssr
          ss22 = [tubo 1.76 ,te ssr ([tubo 3.57 , joelho DRight ] ++ ssr)]
          ss2 = [Gravidade 9.01 , tubo 9.01,joelho DRight , tubo 1.0 , te ss21 ss22 ]
          l0 = [tubo 3.27, te l9 l10 ]
          l9 = [tubo 1.53, joelho DLeft , tubo 1.53 , te [tubo 0.91 ,sp] [tubo 1.39,  te l1 l2]]
          l10 = [tubo 3.28 , te [tubo 0.9,sp] [tubo 2.33 , cruz [tubo 6.40,sp] [tubo 4.6,sp] l11]]
          l11 = [tubo 1.92 , te [tubo 0.9,sp] [tubo 1.4, te [tubo 5.3,sp,tubo 4.2,sp] [tubo 3.5,sp,tubo 4.1 , sp]]]
          l1 = [tubo 1.75 ,sp]
          l2 = [tubo 0.52 , te l3 l4]
          l3 = [tubo 1.66,sp]
          l4 = [tubo 1.9 , te l6 l3]
          l5 = [tubo 1.84,sp]
          l6 = [tubo 1.9,te l7 l8]
          l7 = [tubo 2.88,sp,tubo 1.49,joelho DRight, tubo 1.9,sp]
          l8 = [tubo 0.4,joelho DLeft,tubo 1.2 ,sp , tubo 3.3,sp]
          mainpath = {-opte l01 -} [tubo 3.3,joelho DLeft,tubo 16.1 ,te r5 [tubo 1.54,te r6 (tubo 1.4: r6 )]]
          r8 = Origem [tubo 3.3,joelho DLeft,tubo 16.1 ,te r5 [tubo 1.54,te r6 (tubo 1.4: r6 )]]
          sp = Sprinkler (Just (11,5.8)) 11 densidadeAgua
          tubo d = Tubo Nothing d 100
          tubod di d = Tubo (Just di) d 100
          joelho d = Joelho Nothing ("Conexao","Joelho","90") d 100
          te i j = Te  TeLateral i j
          opte i j = OptTe  TeLateral i j
          cruz i j k = te i [te j k ]
          openpath = sortBy (flip $ comparing fst)  $ fmap (\i -> (pathLength i ,i) ) $  openPath  r8
          result1 = scanl (\j i-> concat $ fmap (\l-> fmap (:l) $ addElement i (head l)) j) (pure $ pure $ initialSprinkler2 vazaoMin) . reverse . init
          renderNode1 name = mapM (\(i,l) -> when (not $ null l ) $  writeFile (name<> backed i <> ".iter" ) . (("\n ----------------- \n Iteration - " ++ show i ++ "\n ----------------- \n" ) ++ ).  L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show (l :: Int) ++ "\n" ++ v) . zip [0..]. fmap (fmap (\i -> if i == '.' then ',' else i ) . L.intercalate "\n") . fmap (fmap (showNode 0)) $ l)  . zip [0 :: Int ..]
   print (head openpath)
   let  r1 = result1 (snd $ head $ openpath)
        firstpass = last $ last r1
        secondpass = [OptionalNode 0 0 ((fmap (Node 0 0 ) [tubod 0.1 10.0] ) ++ firstpass )] ++ firstpass
   -- mapM (\(i,l) -> writeFile ("opt" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  r1
   renderNode1  "pre" $ r1
   renderNode1  "pos" $ result1N $ secondpass
   -- mapM (\(i,l) -> writeFile ("nodeopt" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  last $ result1N  $ last r1

result1N inp = scanl (\j i-> concat $ fmap (\l-> fmap (:l) $ addNode i (head l)) j) ( pure $  pure $ last inp) . reverse . init $ inp

main  = twopass
onepass = do
   let    header = "indice;pressao;vazao;elemento;diametro;comprimento;velocidade\n"
          r0 = [tubo 0.25,tubo 0.50,tubo 0.50,tubo 1.0,tubo 1.0 ,tubo 1.0 ,tubo 1.0,joelho DLeft , tubo 1.78 ,sp]
          r1 = [tubo 1.78,sp]
          r2 = [tubo 0.40,sp,tubo 2.48,sp,tubo 3.10,joelho DLeft ,tubo 1.1,sp]
          r3 = [tubo 2.16,sp]
          r4 = [tubo 2.65,sp]
          r5 = [tubo 0.6,sp]
          r7 = [tubo 1.2,sp,tubo 2.8,sp,tubo 1.7,sp]
          r6 = [tubo 0.3,te [tubo 2.0, te [tubo 1.02,te [tubo 0.59,joelho DRight , tubo 0.48 ,te r1 r0] r4] r2] r3]
          l01 = [Gravidade 26.0 ,tubo 26.0  ]  ++ l0 --  ([Gravidade 2.89,tubo 2.89] ++ l0 )
          ssr = [tubo 1.76,sp,tubo 3.57,sp,tubo 3.57,sp,tubo 3.57,sp]
          ss21 = [tubo 1.76  ,joelho DRight] ++ ssr
          ss22 = [tubo 1.76 ,te ssr ([tubo 3.57 , joelho DRight ] ++ ssr)]
          ss2 = [Gravidade 9.01 , tubo 9.01,joelho DRight , tubo 1.0 , te ss21 ss22 ]
          l0 = [tubo 3.27, te l9 l10 ]
          l9 = [tubo 1.53, joelho DLeft , tubo 1.53 , te [tubo 0.91 ,sp] [tubo 1.39,  te l1 l2]]
          l10 = [tubo 3.28 , te [tubo 0.9,sp] [tubo 2.33 , cruz [tubo 6.40,sp] [tubo 4.6,sp] l11]]
          l11 = [tubo 1.92 , te [tubo 0.9,sp] [tubo 1.4, te [tubo 5.3,sp,tubo 4.2,sp] [tubo 3.5,sp,tubo 4.1 , sp]]]
          l1 = [tubo 1.75 ,sp]
          l2 = [tubo 0.52 , te l3 l4]
          l3 = [tubo 1.66,sp]
          l4 = [tubo 1.9 , te l6 l3]
          l5 = [tubo 1.84,sp]
          l6 = [tubo 1.9,te l7 l8]
          l7 = [tubo 2.88,sp,tubo 1.49,joelho DRight, tubo 1.9,sp]
          l8 = [tubo 0.4,joelho DLeft,tubo 1.2 ,sp , tubo 3.3,sp]
          mainpath = {-opte l01 -} Origem [tubo 3.3,joelho DLeft,tubo 16.1 ,te r5 [tubo 1.54,te r6 (tubo 1.4: r6 )]]
          sp = Sprinkler (Just (11,5.8)) 11 densidadeAgua
          tubo d = Tubo Nothing d 100
          joelho d = Joelho Nothing ("Conexao","Joelho","90") d 100
          te i j = Te  TeLateral i j
          opte i j = OptTe  TeLateral i j
          cruz i j k = te i [te j k ]
          openpath = sortBy (flip $ comparing fst)  $ fmap (\i -> (pathLength i ,i) ) $  openPath mainpath
          result1 = scanl (\j i-> concat $ fmap (\l-> fmap (:l) $ addElement i (head l)) j) (pure $ pure $ initialSprinkler2 vazaoMin) . reverse . init
          renderNode1 = mapM (\(i,l) -> when (not $ null l ) $  writeFile ("res"<> backed i) . (("\n ----------------- \n Iteration - " ++ show i ++ "\n ----------------- \n" ) ++ ).  L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show (l :: Int) ++ "\n" ++ v) . zip [0..]. fmap (fmap (\i -> if i == '.' then ',' else i ) . L.intercalate "\n") . fmap (fmap (showNode 0)) $ l)  . zip [0..].  result1
   print (head openpath)
   mapM (\(i,l) -> writeFile ("opt" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$ last $ result1 (snd $ head $ openpath)
   renderNode1 (snd $ head openpath )


backed i = backedzero i ++ show i
backedzero i
  | i <10 = "000"
  | i <100 = "00"
  | i <1000 = "0"

data Direction
  = DLeft
  | DRight
  deriving(Show)

toCsv (Node p v e ) = show p <|> show v <|> eCsv e
  where
    eCsv (Sprinkler (Just (d,_))  _ _) =  "Sprinkler" <|>  show (d /1000.0)
    eCsv (Sprinkler (Nothing)  _ _) =  "Sprinkler" <|> show (11.0 /1000.0)
    eCsv (Tubo (Just d) c   _) = "Tubo"  <|> show d <|> show c <|> show (v/60/1000/(pi*(d/2)^2))
    eCsv (Joelho (Just d) (i,j,k) _ _) = (i ++ " " ++  j  ++ " " ++ k) <|> show d
    eCsv (Gravidade d ) = "Gravidade" <|> show d

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
  | Gravidade
  { distanciaQueda :: Double
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
  | OptTe
  { tipoTe ::  TeTipo
  , pathRight :: [Element]
  , pathLeft :: [Element]
  }
  | Origem
  { elements :: [Element]
  }
  | OptionalPath
  { pathOption :: [Element]
  }
  | RamalElement
  { pathElement :: [Element]
  }deriving(Show)

data Node a
  = Node
  { pressaoNode :: Double
  , vazao :: Double
  , elemento :: a
  }
  | RamalNode
  { pressaoRamal :: Double
  , vazaoRamal :: Double
  ,pathNode :: [Node a]
  }
  | OptionalNode
  { pressaoRamal :: Double
  , vazaoRamal :: Double
  ,pathNode :: [Node a]
  }deriving(Foldable,Show)


extractElement (Node _ _ e) = e
extractElement (OptionalNode _ _ e) = OptionalPath (fmap extractElement e)
extractElement (RamalNode _ _ e) = RamalElement (fmap extractElement e)

i <|>  j = i ++ ";" ++ j
nodeLists = L.intercalate "\n" . zipWith (\i j -> nodeList i [] j) [0..]
nodeList idx ix (RamalNode v  p l) =  L.intercalate "_" (reverse $ fmap show $ idx:ix)  <|> show v <|> show p <|> "Ramal" ++ "\n"  ++ (L.intercalate "\n" $  zipWith (\ i v-> nodeList i (idx: ix) v ) [0..] l)
nodeList idx ix (OptionalNode v  p l) =  L.intercalate "_" (reverse $ fmap show $ idx:ix)  <|> show v <|> show p <|> "Ramal Opt" ++ "\n"  ++ (L.intercalate "\n" $  zipWith (\ i v-> nodeList i (idx: ix) v ) [0..] l)
nodeList idx ix i =  L.intercalate "_" (reverse $ fmap show $ idx:ix)  <|> toCsv i

pNode (Node p _ _ ) = p
pNode (RamalNode p _ _  ) = p
pNode (OptionalNode p _ _  ) = p
vNode (Node _ v _ ) = v
vNode (RamalNode _ v _  ) = v
vNode (OptionalNode _ v _  ) = v



periodoFuncionamento = 1/24
area d = pi*(d/2)**2
diametrosTubos = [25,32,40,50,65,75,100,125,150]
bicos = [{-(10,3.7),-}(11,5.8){-,(13,8),(14,11.5),(16,15.1)-}]
diametroSeg qseg = 1.3*sqrt qseg *periodoFuncionamento**0.25
pressao q k = (q/k)**2


data Name a = Name a Element

sprinkler (Name n (Sprinkler (Just (i,j))  _ _) )= circle (10*i/1000.0) #  fc red # named n # lw none
segment (Name n (Tubo (Just d) c _)) =fromSegments [ straight (r2 (0, c))] # lw  (Output d) # lc red


