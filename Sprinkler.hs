{-# LANGUAGE DeriveFoldable,TupleSections,NoMonomorphismRestriction #-}
import Control.Monad
import Numeric.GSL.Minimization
import Numeric.GSL.Root
import Numeric
import Data.Ord
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Foldable as F
import Data.Foldable ( Foldable)
import Data.Packed.Matrix(Matrix)
import Data.List as L
import qualified Data.Traversable as Tra
import qualified Data.Map as M
import Control.Applicative hiding ((<|>))
import Debug.Trace

justError e Nothing = error e
justError _ (Just i) = i

minPressure =(\i -> if null i then [] else pure . minimumBy (comparing pNode) $ i)

minFlow =(\i -> if null i then [] else pure . minimumBy (comparing vNode) $ i)

traceEmpty v [] =  traceShow v []
traceEmpty _ i = id i

trace2Empty v [] [] =  []
trace2Empty v i  [] =  traceShow v []
trace2Empty _ _ i = id i

-- solveRamalN :: Node Element -> Node Element -> [Node Element]
solveRamalN s@(RamalNode  _ _  e )  n = minFlow $   do
  let
      pinit = pNode n
      vinit = vNode n
      sp@(Node _ vlast es) = last e
      sp2 si v =  calcSprinkler v ssp
        where (Node _ _ ssp)= si
      fun2 spi [v] = if null res then pinit  else minimum  res
          where res = fmap abs $ fmap (\i ->  i - pinit )  (fmap pNode $ foldr (\i j->  concat $ mapM (addNode i) j) [sp2 spi v] (init e))
      minimizer spi  = minimize NMSimplex 1e-2 100 (fmap (*0.6) initvalue ) (fun2 spi)  initvalue
      initvalue = [(vminSprinkler es) ]
      ([vmin],res) =  minimizer ( sp)
      spmin =  sp2 sp vmin
  path@((Node plast vlast _): _)  <-  foldr (\i j-> concat $ mapM (\l -> fmap (:l)  $ addNode i (head l) ) j) [[spmin]] (init e)
  guard $   (abs $ plast - pinit ) < 1e-1
  return  (Node pinit vlast path)

addNode (Node i j e ) n = addElement e  n

addNode r@(RamalNode _ _  _ ) n = do
  let pinit = pNode n
      vinit = vNode n
  (Node pinit vlast res) <- solveRamalN r n
  return (RamalNode pinit (vinit + vlast ) res )

addNode (OptionalNode p v  r) n = do
  let pinit = pNode n
      vinit = vNode n
  (Node pinit vlast res) <- solveRamalN (RamalNode p v r) n
  return (OptionalNode pinit (max vinit  vlast ) res )

solveBifurcation (Te _ l rl) n@(Node pinit vinit o) =  do
  let
      buildPaths invr =do
             let res1 =  deConstructPath l n1
                 res2 =  deConstructPath rl n2
                 n1 = Node pinit (r1*vinit) o
                 n2 = Node pinit (r2*vinit) o
                 r1 = if invr <= 0 then 1 else 1/invr
                 r2 = 1 - r1
             v1 <- res1
             v2 <- res2
             return $ (v1 , v2)
      fun2 [r] =  (\res -> if null res then 0 else minimum res ) $ fmap (\(v1,v2) ->  abs ( (pressaoNode . head) v1 - (pressaoNode . head) v2)) $buildPaths r
      minimizer = minimize NMSimplex2 1e-4 140  (fmap (*0.9) initvalue ) (fun2 )  initvalue
      initvalue = [1]
      ([rmin],res) =  minimizer
  ep@(path1@((Node plast vlast _): _), path2@((Node plast2 vlast2 _): _)) <- buildPaths rmin
  guard   {-traceShow (rmin,plast,plast2,res)-} $ (abs $ plast - plast2 ) < 1e-2
  return  $  (RamalNode plast (vlast + vlast2) path1):  path2

solveRamal s@(RamalElement l)  n@(Node pinit vinit o) =  do
  let
      sp = minSprinkler $ last l
      sp2 si v =  calcSprinkler v ssp
        where (Node _ _ ssp)= si
      fun2 sp [v] =  if null res then pinit  else minimum  res
          where res =  fmap abs $ fmap (pinit-)  (fmap pressaoNode $ foldr (\i j->  concat $ mapM (addElement i) j) [sp2 sp v] (init l))
      minimizer sp  = minimize NMSimplex 1e-2 100  (fmap (*0.4) initvalue ) (fun2 sp)  initvalue
      initvalue = [vminSprinkler (last l)]
  let spmin = let ([vmin],_) =  minimizer sp
                    in sp2 sp vmin
  path@((Node plast vlast _): _)  <- foldr (\i j-> concat $ mapM (\l -> fmap (:l)  $ addElement i (head l) ) j) [[spmin]] (init l)
  guard $  (abs $ plast - pinit ) < 1e-1
  return  (Node pinit vlast path)

nodePlus n1 (Node p v e) = Node (pNode n1 + p)  (vNode n1 + v ) e
nodePlus n1 (RamalNode p v e) = RamalNode (pNode n1 + p)  (vNode n1 + v ) e

nodeSub n1 (Node p v e) = Node (pNode n1 - p)  (vNode n1 - v ) e
nodeSub n1 (RamalNode p v e) = RamalNode (pNode n1 - p)  (vNode n1 - v ) e

perdatubo t  p  v =  (Node (p + perda*10) v t)
        where a = area
              (Just d ) = diametroE t
              c = materialE t
              vazao = v/1000/60
              -- NOTE : Abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(abs (vazao)**1.85)*(distanciaE t)/((c**1.85)*(d**4.87))


perdatubo' t  p  v =  (Node (perda*10) 0 t)
        where a = area
              (Just d ) = diametroE t
              c = materialE t
              vazao = v/1000/60
              -- NOTE : Abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(abs (vazao)**1.85)*(distanciaE t)/((c**1.85)*(d**4.87))

minimumTubo v = minimumBy (comparing (abs . ((diametroSeg (v/1000/60))-) )) $ fmap (/1000) diametrosTubos


diffElement j@(Joelho preD tipo jd c)  nodo = do
    let v = vNode nodo
        p = pNode nodo
        e = elementE nodo
    return $ case preD of
      Just dtubom -> perdatubo' j p v
      Nothing -> case join $ fmap diametroE e of
            Just d -> perdatubo' (Joelho (Just d)  tipo jd c ) p v
            Nothing ->  perdatubo' (Joelho (Just $ minimumTubo v ) tipo jd c) p v

diffElement t@(Tubo preD distancia c)  nodo = do
    let v = vNode nodo
        p = pNode nodo
        e = elementE nodo
    return $ case preD of
         Just _ -> perdatubo' t p v
         Nothing ->  case join $ fmap diametroE e of
                Just d -> perdatubo' (Tubo (Just d) distancia c) p v
                Nothing ->  perdatubo' (Tubo (Just $ minimumTubo v ) distancia c) p v

diffElement g@(Gravidade d) n = do
  return (Node (negate $  d*10 ) 0  g)

diffElement s@(Sprinkler i  area densidadeMin)  n = do
  let v = vNode n
      p = pNode n
  case  i of
       Just (d,k1,dm) -> do
            let
                pn = p - pv v dm
            return $ Node p (v +  k1*sqrt p)  s
       Nothing ->  do
            b@(d,k1) <- bicos
            let
                dm = minimumTubo v
                pn = p - pv v dm
            return $ Node 0 ( k1*sqrt pn)  (Sprinkler (Just (d,k1,dm)) area densidadeMin)

diffElement s@(RamalElement l) n = minFlow $ do
  let vinit = vNode n
      pinit = pNode n
  (Node pinit vlast res) <- solveRamal s n
  return (RamalNode 0 vlast  res )

diffElement i j = error ("No match for addElement " <> show i <> "   " <> show j)

-- addElement j nodo = fmap (nodePlus nodo) (diffElement j nodo)
removeElement j nodo = fmap (nodeSub nodo) (diffElement j nodo)


addElement j@(Joelho preD tipo jd c)  nodo = do
    let v = vNode nodo
        p = pNode nodo
        e = elementE nodo
    return $ case preD of
      Just dtubom -> perdatubo j p v
      Nothing -> case join $ fmap diametroE e of
            Just d -> perdatubo (Joelho (Just d)  tipo jd c ) p v
            Nothing ->  perdatubo (Joelho (Just $ minimumTubo v ) tipo jd c) p v

addElement t@(Tubo preD distancia c)  nodo = do
    let v = vNode nodo
        p = pNode nodo
        e = elementE nodo
    return $ case preD of
         Just _ -> perdatubo t p v
         Nothing ->  case join $ fmap diametroE e of
                Just d -> perdatubo (Tubo (Just d) distancia c) p v
                Nothing ->  perdatubo (Tubo (Just $ minimumTubo v ) distancia c) p v

addElement g@(Gravidade d) n = do
  return (Node ((pressaoNode n) - d*10 ) (vazao n) g)

addElement s@(Sprinkler i  area densidadeMin)  n = do
  let v = vNode n
      p = pNode n
  case  i of
       Just (d,k1,dm) -> do
            let
                pn = p - pv v dm
            return $ Node p (v +  k1*sqrt p)  s
       Nothing ->  do
            b@(d,k1) <- bicos
            let
                dm = minimumTubo v
                pn = p - pv v dm
            return $ Node p (v +  k1*sqrt pn)  (Sprinkler (Just (d,k1,dm)) area densidadeMin)

addElement s@(OptionalPath i) n = minFlow $ do
  let vinit = vNode n
      pinit = pNode n
  nr@(Node pr vr res ) <- solveRamal (RamalElement i) n
  return $ (OptionalNode pinit (max vr vinit) res)

addElement s@(RamalElement l) n = minFlow $ do
  let vinit = vNode n
      pinit = pNode n
  (Node pinit vlast res) <- solveRamal s n
  return (RamalNode pinit (vinit + vlast ) res )

addElement i j = error ("No match for addElement " <> show i <> "   " <> show j)


pv :: Double -> Double -> Double
pv v d =  225*v^2/(d*1000.0) ^4

showNode i n@(Node _ _ _) = (concat $ replicate i "\t") ++ show n
showNode i (RamalNode p v n ) = (concat $ replicate i "\t") ++ "RamalNode " ++ show p ++ "  " ++ show v ++ {-"  " ++ show res ++-} "\n" ++ (L.intercalate "\n" $ fmap (showNode (i +1))  n)
showNode i (OptionalNode p v n ) = (concat $ replicate i "\t") ++ "OptionalNode" ++ show p ++ "  " ++ show v ++ {-"  " ++ show res ++-} "\n" ++ (L.intercalate "\n" $ fmap (showNode (i +1))  n)

{-
pathData = [RamalElement ramal,Joelho Nothing ("Conexao","Te","Lateral") 130 ,  Tubo Nothing 3.3 130 , RamalElement ramal , Tubo Nothing 3.3 130,Joelho Nothing ("Conexao","Joelho","90") 130, Tubo Nothing 12.40 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.6 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130]

ramal = [Joelho Nothing ("Conexao","Joelho","90") 130,Tubo Nothing 12.4 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.6 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua ,Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua, Tubo Nothing 3.60 130,Sprinkler 8.0 coberturaChuveiro densidadeAgua]
-}

initialSprinkler e@(Sprinkler _ area den)  =  do
  b@(d,k) <- bicos
  let
      vmin = vminSprinkler e
      dm = minimumTubo  vmin
      p = pv vmin dm  + pressao vmin k
  guard $ p > 50
  guard $ vmin >= area*den
  return (Node p vmin (Sprinkler (Just (d,k,dm)) area den))

calcSprinkler v e@(Sprinkler (Just (d,k,dm)) _ _) = (Node (pv v dm + pressao v k) v e)

minSprinkler e@(Sprinkler (Just (d,k,dm)) a den) = (Node (pv vmin dm + pressao vmin k) vmin e)
  where vmin = vminSprinkler e

vminSprinkler e@(Sprinkler _ a den) =  a * den


deConstructPath  i v = pp
    where pp = foldr (\ n m -> concat $ fmap (\l -> fmap (:l) $ removeElement n (head l)) m) (backPressurePath (last i) v) (init i)
deConstructPath' i v = join $ fmap (\ppi-> fmap (tail ppi ++ ) . backPressurePath (last i) . last $ ppi) pp
    where pp = foldl (\ m n -> concat $ fmap (\l -> fmap (\ll -> l ++ [ll]) $ removeElement n (last l)) m) [[v]] (init i)

backPressurePath (Origem i ) n@(Node v p e ) =  filter (not .null) $ deConstructPath i n
backPressurePath t@(Te _  i j ) n@(Node v p e ) =  filter (not .null) $ solveBifurcation t  n
backPressurePath  b@(Bocal d)  n@(Node v p e) = return $ join $ unConsPath (Joelho d ("Bocais","Saida","Normal")  DRight 100)  [Node v p b]
-- backPressurePath  Bocal  n@(Node v p e) = return $  [Node v p Bocal]
backPressurePath l m = error $ "no pattern for backPressure" ++ show l ++ " ____ " ++show m

-- Expand  main path
constructPath i= foldr (\n m -> concat $ fmap (\l -> fmap (:l) $ addElement n (head l)) m) ( pressurePath (last i )) (init i)


consPath n l =  fmap (:l) $ addElement n (head l)
unConsPath n l =  fmap (:l) $ removeElement n (head l)

backSolveN c assoc left right = do
          let li = (head left)
              ri = (head right)
              te d = Joelho Nothing ("Conexao","Te","Lateral") d 100
          (Node pn vn res) <- solveRamalN (RamalNode (pNode li ) ((vNode li ) `assoc`  (vNode ri )) left) ri
          return (c pn (vn `assoc` vNode ri) res : right)




backSolveE c assoc left right = do
          let li = (head left)
              ri = (head right)
              te d = Joelho Nothing ("Conexao","Te","Lateral") d 100
          (Node pn vn res) <- solveRamal (RamalElement $ fmap extractElement left) ri
          return   (c pn (vn `assoc` vNode ri) res : right)



bifurcationSolve  solve i j = filter (not .null) $do
  left <- constructPath i
  right <- constructPath j
  let cond li ri
        | pNode ri > pNode li =  solve left right
        | otherwise = solve right left
  return $ join $ cond (head left) (head right)



pressurePath :: Element -> [[Node Element]]
pressurePath (Origem i) = filter (not.null) $ constructPath i
pressurePath (Te _ i j) = bifurcationSolve (\l -> join . fmap (consPath (te DRight)) . backSolveE RamalNode (+) l )  i j

-- pressurePath (OptTe _ i j) = bifurcationSolve (\l -> join . fmap (consPath (te DRight)) .backSolveN OptionalNode max l) i j
pressurePath (OptTe _ i j) = join $ join [consPath (Joelho Nothing ("Conexao","Te","Lateral" ) DRight 100 ) <$> constructPath i , consPath (Joelho Nothing ("Conexao","Te","Direta" ) DRight 100 ) <$> constructPath j]
pressurePath s@(Sprinkler _ _ _) = fmap pure $ initialSprinkler s
pressurePath (Reservatorio t v b) = do
  let vmax =  maximumBy (comparing (vazao .head))  pb
      pb = pressurePath b
  i <- pb
  return $ (Node 0 (vazao $ head i) $Reservatorio t (Just $ (vazao . head $ vmax)*t) b ) : i
pressurePath g@(Bomba _  e suc )  = do
  let
      rawPaths =  do
         p <- constructPath e
         bp <- backPressurePath (Origem suc) (Node 0 (negate $ vazao $head p) Tip )
         return (p,bp)
      minBomb (p,v) = minimumBy (comparing (\(i,j) -> (i - p,j - v))) $ filter (\(i,j) -> i - p > 0 && j - v> 0 ) $  bomb
      calcBomb (p,bp) = traceShowId   (pNode (head p) - pNode (head bp), vNode (head p))
      (minp,minv) = maximum $ fmap (minBomb.calcBomb) rawPaths
      pmin v = minp*(bombaSuccaoFrontal (v/minv*100))/100
      fun rawPath [v] = (\res -> if null res then v else minimum res) $ (abs.(v-) .vazao) <$> ( solveRamalN (RamalNode pm v (fst rawPath)) (Node (traceShowId pm) v Tip))
          where pm = pmin v + (traceShowId $ pressaoNode (head $ (\(i:xs)-> i) $  (backPressurePath (Origem suc) (Node 0 (-v) Tip))))
      minFun rawPath = (fst $ rawPath,fst  $  minimize NMSimplex 1e-3 60  [0.9*minv] (fun rawPath ) [minv])
  (path,[vres]) <- fmap (minFun ) rawPaths
  bp <- backPressurePath (Origem suc ) (Node 0 (-vres) Tip)
  n@(Node p v res) <- solveRamalN  (RamalNode (pmin vres) vres path) (Node (pmin vres + pressaoNode (head $  bp )) vres Tip)
  return $  RamalNode (pressaoNode (head bp))  v bp : Node (pressaoNode (head bp))  v (Bomba (Just (minp,minv)) e suc ) :  res
pressurePath i = error $ "pressure path no match " ++ show i

te d = Joelho Nothing ("Conexao","Te","Lateral") d 100

header = "indice;pressão;vazão; variação pressão;variação vazão;elemento;diâmetro;comprimento;velocidade;diâmetro estimado;area;densidade sprinkler;densidade sprinkler mínima\n" ++ headerunit
headerunit= ";kPa;l/min;kPa;l/min;;m;m;m/s;m;m²;l/(min.m²);l/(min.m²)\n"

writeExcelCsv n r1 = mapM (\(i,l) -> T.writeFile (n ++ show i ++ ".csv" ) (T.decodeLatin1 $ BS.pack $ header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). diffLists) )$  r1
writeCsv r1 = mapM (\(i,l) -> writeFile ("westpoint" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). diffLists) )$  pressurePath r1

westpoint = do
  let r1 = Origem [tubo 1.31,joelho,tubo 3.22,teDireto,tubo 1.22,joelho,tubo 2.8,teDireto ,tubo 2.23,teDireto, tubo 1.67 , te [ tubo 0.73,sp] [tubo 0.54,teDireto, tubo 2.96 , cruz [tubo 1.77,sp] [tubo 4.0 , te [tubo 1.77 ,sp] r2] r3]]
      r2 = [tubo 2.18 ,sp,tubo 4.0,sp,tubo 4.0 ,sp,tubo 4.0 ,sp , tubo 1.50, joelho45, tubo 1.41,sp ]
      r3 = [tubo 2.18 ,sp,tubo 4.0,sp,tubo 4.0 ,sp,tubo 4.0 ,sp ,tubo 2.50 ,sp]
      sp = Sprinkler (Just (11,5.8,25)) 11 4.1
      spo = Sprinkler (Just (11,5.8,25)) 12 6.1
      tubo d = Tubo Nothing d 100
      tubod di d = Tubo (Just di) d 100
      joelho  = Joelho Nothing ("Conexao","Joelho","90") DRight  100
      joelho45  = Joelho Nothing ("Conexao","Joelho","45") DRight  100
      teLat  = Joelho Nothing ("Conexao","Te","Lateral") DRight  100
      teDireto = Joelho Nothing ("Conexao","Te","Direta") DRight  100
      te i j = Te  TeLateral i j
      opte i j = OptTe  TeLateral i j
      cruz i j k = te i [te j k ]
  writeExcelCsv "westpoint" (pressurePath r1)

twopass = do
   let
          r0 = [tubo 5.25 ,joelho DLeft , tubo 1.78 ,sp]
          r1 = [tubo 1.78,sp]
          r2 = [tubo 0.40,sp,tubo 2.48,sp,tubo 3.10,joelho DLeft ,tubo 1.1,sp]
          r3 = [tubo 2.16,sp]
          r4 = [tubo 2.65,sp]
          r5 = [tubo 0.6,sp]
          r6 = [tubo 0.3,te [tubo 2.0, te [tubo 1.02,te [tubo 0.59,joelho DRight , tubo 0.48 ,te r1 r0] r4] r2] r3]
          r7 = [tubo 1.2,sp,tubo 2.8,sp,tubo 1.7,sp]
          r8 = [tubo 3.27,joelho DLeft,tubo 16.1 ,te r5 [tubo 1.54,te r6 (tubo 1.4: r6 )]]

          ssr = [tubo 1.76,spo,tubo 3.57,spo ,tubo 3.57,spo,tubo 3.57,spo]
          ss21 = [tubo 1.76  ,joelho DRight] ++ ssr
          ss22 = [tubo 1.76 ,te ssr ([tubo 3.57 , joelho DRight ] ++ ssr)]
          ss2 = [joelho DRight , teDireta, gaveta, tubo 1.0 , te ss21 ss22 ]

          l0 = [tubo 3.27, te l9 l10 ]
          l1 = [tubo 1.75 ,sp]
          l2 = [tubo 0.52 , te l3 l4]
          l3 = [tubo 1.66,sp]
          l4 = [tubo 1.9 , te l6 l3]
          l5 = [tubo 1.84,sp]
          l6 = [tubo 1.9,te l7 l8]
          l7 = [tubo 2.88,sp,tubo 1.49,joelho DRight, tubo 1.9,sp]
          l8 = [tubo 0.4,joelho DLeft,tubo 1.2 ,sp , tubo 3.3,sp]
          l9 = [tubo 1.53, joelho DLeft , tubo 1.53 , te [tubo 0.91 ,sp] [tubo 1.39,  te l1 l2]]
          l10 = [tubo 3.28 , te [tubo 0.9,sp] [tubo 2.33 , cruz [tubo 6.40,sp] [tubo 4.6,sp] l11]]
          l11 = [tubo 1.92 , te [tubo 0.9,sp] [tubo 1.4, te [tubo 5.3,sp,tubo 4.2,sp] [tubo 3.5,sp,tubo 4.1 , sp]]]
          sp = Sprinkler (Just (11,5.8,25)) 11 4.1
          spo = Sprinkler (Just (11,5.8,25)) 12 6.1
          tubo d = Tubo Nothing d 100
          retencao = Joelho Nothing ("Valvula","Retencao","Horizontal") DRight 100
          gaveta = Joelho Nothing ("Valvula","Aberta","Gaveta") DRight 100
          tubod di d = Tubo (Just di) d 100
          joelho d = Joelho Nothing ("Conexao","Joelho","90") d 100
          teLat d = Joelho Nothing ("Conexao","Te","Lateral") d 100
          teDireta  = Joelho Nothing ("Conexao","Te","Direta") DRight 100
          te i j = Te  TeLateral i j
          opte i j = OptTe  TeLateral i j
          cruz i j k = te i [te j k ]
          renderNode1 name = mapM (\(i,l) -> when (not $ null l ) $  writeFile (name<> backed i <> ".iter" ) . (("\n ----------------- \n Iteration - " ++ show i ++ "\n ----------------- \n" ) ++ ).  L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show (l :: Int) ++ "\n" ++ v) . zip [0..]. fmap (fmap (\i -> if i == '.' then ',' else i ) . L.intercalate "\n") . fmap (fmap (showNode 0)) $ l)  . zip [0 :: Int ..]
   let  r1 = pressurePath (Origem r8 )
        s1 = pressurePath  (Origem ss2)
        req = [teDireta,gaveta,  OptTe TeLateral r8  l0]
        t21 = pressurePath (Origem l0)
        firstpass = last $ last r1
        succao2 = [tubod 0.1 1.6,joelho DRight, tubod 0.1 1.0,joelho DRight , tubod 0.1 1.6 , Gravidade (1.0),te [gaveta,tubod 0.1 0.4,Gravidade (0.4),Bocal (Just 0.1)] [gaveta,tubod  0.1 0.8 ,joelho DRight ,tubod 0.1 0.4,Gravidade (0.4),Bocal (Just 0.1)]]
        succao = [tubod 0.1 1.6,gaveta,retencao,joelho DRight, tubod 0.1 1.0,joelho DRight , tubod 0.1 1.6 , tubo 0.8 ,joelho DRight ,tubo 0.4]
        recalque = {-succao ++ -} [tubod 0.1 1.2 ,Gravidade (-1.2) , retencao , gaveta,joelho DRight , tubo 0.5, teLat DRight ,tubo 3 ,joelho DRight , tubo 4, joelho DRight , tubo 3.2, joelho DRight , tubod 0.1 5.43,Gravidade 5.43]
        bombaTipo12 =  Reservatorio 30 Nothing $ Bomba Nothing  (recalque ++ [ subsolo] ) succao2
        tipo12=  req
        tipo1=  Origem ([tubod 0.1 (2.89*11),Gravidade (2.89*11) ] ++ l0 )
        subsolo=  OptTe  TeLateral req  (replicate 10 teDireta ++ [ tubod 0.1 (2.89*11),Gravidade (2.89*11) ,opte l0 (replicate 2 teDireta ++ [ tubod 0.1 (3*2.89),Gravidade (3*2.89)  ] ++ ss2)])
   --renderNode1 "ppath" [(pressurePath $ Origem ss2)]
   --renderNode1 "tipo-12" [pressurePath tipo12]
   --renderNode1 "tipo-1" [pressurePath tipo1]
   --renderNode1  "subsolo" $ [pressurePath $ subsolo]
   -- mapM (\(i,l) -> writeFile ("tipo1" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath tipo1
   -- mapM (\(i,l) -> writeFile ("tipo12" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath tipo12
   -- mapM (\(i,l) -> writeFile ("subsolo" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath subsolo
   -- writeExcelCsv "himalaia" $  backPressurePath (Origem succao2) (Node 0 (-1300) (joelho DRight))
   writeExcelCsv "himalaia" $  pressurePath  bombaTipo12
   --mapM (\(i,l) -> writeFile ("bombaTipo1" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath bombaTipo1
   -- mapM (\(i,l) -> writeFile ("bombaSubsolo" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath bombaSubsolo
   {-renderNode1 "t21" t21
   renderNode1 "r1" r1
   -- renderNode1  "r" $  result1N req
   -- mapM (\(i,l) -> writeFile ("andar_12" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  last $ result1N req
-}

result1N inp = scanl (\j i-> concat $ fmap (\l-> fmap (:l) $ addNode i (head l)) j) ( pure $  pure $ last inp) . reverse . init $ inp

main  = twopass

backed i = backedzero i ++ show i
backedzero i
  | i <10 = "000"
  | i <100 = "00"
  | i <1000 = "0"

data Direction
  = DLeft
  | DRight
  deriving(Eq,Show)

showJust f (Just i ) = f i
showJust _ Nothing = ""

data TeTipo
  = TeLateral
  | TeFrontal
  deriving(Eq,Show)

data Element
  = Tubo
  { diametro :: Maybe Double
  , comprimento :: Double
  , atrito :: Double
  }
  | Gravidade
  { distanciaQueda :: Double
  }
  | Reservatorio
  { tempo :: Double
  , volume :: Maybe Double
  , element :: Element
  }
  | Tip
  | Bocal
  { diametroJ :: Maybe Double}
  | Bomba
  { nominalValues :: Maybe (Double,Double)
  , recalqueElements :: [Element]
  , succaoElements :: [Element]
  }
  | Joelho
  { diametroJ :: Maybe Double
  , tipoJ :: (String,String,String)
  , direction :: Direction
  , material :: Double
  }
  | Sprinkler
  { tipo :: Maybe (Double,Double,Double)
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
  }deriving(Eq,Show)

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


extractElement (Node _ _ e) = e
extractElement (OptionalNode _ _ e) = OptionalPath (fmap extractElement e)
extractElement (RamalNode _ _ e) = RamalElement (fmap extractElement e)

i <|>  j = i ++ ";" ++ j

diffLists  els = L.intercalate "\n" . fst $ foldr (\(i,v) (s,vl)-> (diffList i [] v vl:s,v) ) ([], Node 0 0 Tip) $ zip  [0..] els
diffList idx ix (RamalNode p  v l) nl  = L.intercalate "_" (reverse $ fmap show $ idx:ix)  <|> formatFloatN 1 p <|> formatFloatN 1 v <|> formatFloatN 1 (p - pressaoNode nl) <|> formatFloatN 1 (v - vazao nl) <|>"Ramal" ++ "\n"  ++ (L.intercalate "\n" $  fst $ foldr (\(i,v) (s,vl)-> (diffList i (idx: ix) v vl:s,v) ) ([], Node 0 0 Tip) $ zip  [0..] l)
diffList idx ix i nl = L.intercalate "_" (reverse $ fmap show $ idx:ix)  <|> diffCsv i nl

diffCsv (Node p v e ) nl  = formatFloatN 3 p <|> formatFloatN 3 v <|> formatFloatN 1 dp <|> formatFloatN 1 dv  <|> eCsv e
  where
    lp = pNode nl
    lv = vNode nl
    dv = v - lv
    dp = p - lp
    eCsv sp@(Sprinkler (Just (d,_,_))  a den) =  "Sprinkler" <|>  formatFloatN 3 (d/1000.0) <|> "" <|> "" <|> "" <|> show a <|> formatFloatN 2 (dv / a) <|> formatFloatN 2 den
    eCsv (Tubo (Just d) c   _) = "Tubo"  <|> formatFloatN 3 d <|> show c  <|> formatFloatN 2 (v/60/1000/(pi*(d/2)^2))<|> formatFloatN 3 (diametroSeg ((abs v)/1000.0/60.0))
    eCsv jo@(Joelho (Just d) (i,j,k) _ _) = (i ++ " " ++  j  ++ " " ++ k) <|> showJust (formatFloatN 3) (diametroE jo) <|> show (distanciaE jo)
    eCsv (Gravidade d ) = "Gravidade" <|> "" <|> show d
    eCsv (Bomba  (Just (p, v)) _   _ ) = "Bomba "  <> formatFloatN 1 p <> " kpa - " <> formatFloatN 1 v <> " L/min"
    eCsv (Reservatorio t (Just v) _ ) = "Reservatorio "  <> formatFloatN 0 t <> " min - " <> formatFloatN 0 v <> " L"
    eCsv (Bocal _) = "Bocal"


pNode (Node p _ _ ) = p
pNode (RamalNode p _ _  ) = p
pNode (OptionalNode p _ _  ) = p
vNode (Node _ v _ ) = v
vNode (RamalNode _ v _  ) = v
vNode (OptionalNode _ v _  ) = v

diametroE :: Element -> Maybe Double
diametroE (Tubo d _ _ ) = d
diametroE (Joelho d _ _ _) = d
diametroE i = Nothing


distanciaE :: Element -> Double
distanciaE (Tubo _ d _ ) = d
distanciaE (Joelho (Just dtubom) tipo _ c) =  justError (show (tipo,c, dtubom*1000)) $ join $ fmap (M.lookup (dtubom*1000)) $ M.lookup (tipo,c) joelhos

materialE :: Element -> Double
materialE (Tubo _ _ c) =  c
materialE (Joelho _ _ _ c) = c
materialE i = error $ "No Material Defined" ++ show i

elementE (Node _ _ e) = Just e
elementE i = Nothing


bombaSuccaoFrontal, bombaBipartida :: Double -> Double
bombaSuccaoFrontal x = 151.229 - 0.422331*x - 0.000979227*x^2 - 1.194467786948394e-7*x^3
bombaBipartida x = 120 + 0.0142857*x - 0.00134921*x^2 - 7.936507936507936e-6*x^3

periodoFuncionamento = 1/24
area d = pi*(d/2)**2
diametrosTubos = [25,32,40,50,65,75,100,125,150]
bicos = [{-(10,3.7),-}(11,5.8){-,(13,8),(14,11.5),(16,15.1)-}]
diametroSeg qseg = 1.3*sqrt (abs qseg) *periodoFuncionamento**0.25
pressao q k = (q/k)**2

risco = [("Leve",c1),("Ordinário I",c2),("Ordinário II",c3),("Risco Extra Ordinário I",c4),("Risco Extra Ordinário II", c5)]
  where
    c1 = regr (2.8,279) (4.1,140)
    c2 = regr (2.8,279) (4.1,140)
    c3 = regr (2.8,279) (4.1,140)
    c4 = regr (2.8,279) (4.1,140)
    c5 = regr (2.8,279) (4.1,140)

linFun (a,b) x = a*x + b
invLinFun (a,b) x = (x - b)/a
regr (x,fx) (y,fy)  =  (a, b)
            where a = (fx - fy)/(x - y)
                  b = fx - x*a

joelhos = M.fromList
    [((("Conexao","Joelho","90"),130),M.fromList [(32,1.5),(40,3.2),(50,3.4),(65,3.7)])
    ,((("Conexao","Joelho","90"),100),M.fromList [(25,0.8),(32,1.1),(40,1.3),(50,1.7),(65,2.0),(75,2.5),(100,3.4),(125,4.2),(150,4.9)])
    ,((("Valvula","Aberta","Gaveta"),100),M.fromList [(25,0.2),(32,0.2),(40,0.3),(50,0.4),(65,0.4),(75,0.5),(100,0.7),(125,0.9),(150,1.1)])
    ,((("Bocais","Saida","Normal"),100),M.fromList [(25,0.7),(32,0.9),(40,1.0),(50,1.5),(65,1.9),(75,2.2),(100,3.2),(125,4.0),(150,5.0)])
    ,((("Valvula","Retencao","Horizontal"),100),M.fromList [(25,2.1),(32,2.7),(40,3.2),(50,4.2),(65,5.2),(75,6.3),(100,8.4),(125,10.4),(150,12.5)])
    ,((("Conexao","Joelho","45"),100),M.fromList [(25,0.4),(32,0.5),(40,0.6),(50,0.8),(65,0.9),(75,1.2),(100,1.5),(125,1.9),(150,2.3)])
    ,((("Conexao","Te","Lateral"),130),M.fromList [(32,4.6),(40,7.3),(50,7.6),(65,7.8),(75,8.0)])
    ,((("Conexao","Te","Direta"),100),M.fromList [(25,0.5),(32,0.7),(40,0.9),(50,1.1),(65,1.3),(75,1.6),(100,2.1),(125,2.7),(150,3.4)])
    ,((("Conexao","Te","Lateral"),100),M.fromList [(25,1.7),(32,2.3),(40,2.8),(50,3.5),(65,4.3),(75,5.2),(100,6.7),(125,8.4),(150,10.0)])]

bomb =fmap (\(p,v)-> (p,v*1000.0/60)) [(250,60),(250,70),(250,80),(250,90),(300,60),(300,70),(300,80),(300,90),(350,60),(350,70),(350,80),(350,90),(400,60),(400 ,70),(400,80),(400,90),(450,60),(450,70), (450,80),(450,90)]

formatFloatN numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

