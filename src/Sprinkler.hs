{-# LANGUAGE DeriveFoldable,TupleSections#-}
module Sprinkler (unrollNode,editDiametro ,formatFloatN )where

import Control.Monad
import Data.Tuple
import Control.Monad.Trans.State
import Numeric.GSL.Minimization
import Numeric.GSL.Root
import Numeric
import Data.Ord
import Element
import Data.Maybe
import Data.Monoid
import Tee
import Prelude hiding(null)
import qualified Data.Foldable as F
import Data.Foldable ( Foldable)
-- import Data.Packed.Matrix(Matrix)
import qualified Data.List as L
import Data.List (minimumBy,maximumBy)
import qualified Data.Traversable as Tra
import qualified Data.Map as M
import Control.Applicative hiding ((<|>))
import Debug.Trace

null :: [a] -> Bool
null = L.null

{-
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
      minimizer spi  = minimize NMSimplex 1e-3 100 (fmap (*0.6) initvalue ) (fun2 spi)  initvalue
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

solveBifurcation (Te _ _ l rl) n@(Node pinit vinit o) =  do
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

perdatubo t  p  v =  (Node (p + perda*10) v t)
        where
              (Just d ) = diametroE t
              c = materialE t
              vazao = v/1000/60
              -- NOTE : Abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(abs (vazao)**1.85)*(distanciaE t)/((c**1.85)*(d**4.87))


perdatubo' t  v =  (Node (perda*10) 0 t)
        where
              (Just d ) = diametroE t
              c = materialE t
              vazao = v/1000/60
              -- NOTE : Abs na vazão pois gera NaNs para valores negativos durante iterações
              perda = 10.65*(abs (vazao)**1.85)*(distanciaE t)/((c**1.85)*(d**4.87))

minimumTubo v = minimumBy (comparing (abs . ((diametroSeg (v/1000/60))-) )) $ fmap (/1000) diametrosTubos
-}
{-
diffElement j@(Joelho preD tipo jd c)  nodo = do
    let v = vNode nodo
        p = pNode nodo
        e = elementE nodo
    return $ case preD of
      Just dtubom -> perdatubo' j  v
      Nothing -> case join $ fmap diametroE e of
            Just d -> perdatubo' (Joelho (Just d)  tipo jd c )  v
            Nothing ->  perdatubo' (Joelho (Just $ minimumTubo v ) tipo jd c)  v

diffElement t@(Tubo preD distancia c)  nodo = do
    let v = vNode nodo
        p = pNode nodo
        e = elementE nodo
    return $ case preD of
         Just _ -> perdatubo' t v
         Nothing ->  case join $ fmap diametroE e of
                Just d -> perdatubo' (Tubo (Just d) distancia c)  v
                Nothing ->  perdatubo' (Tubo (Just $ minimumTubo v ) distancia c)  v

diffElement g@(Gravidade d) n = do
  return (Node (negate $  d*10 ) 0  g)

diffElement s@(Sprinkler i  mdm area densidadeMin)  n = do
  let v = vNode n
      p = pNode n
  case  i of
       Just (d,k1) -> do
            let
                dm = maybe (minimumTubo (v + k1*sqrt p)) id mdm
                pn = p - pv v (dm*1000.0)
            return $ Node 0 ( k1*sqrt pn)  s
       Nothing ->  do
            b@(d,k1) <- bicos
            let
                dm = maybe (minimumTubo (v + k1*sqrt p)) id mdm
                pn = p - pv v (dm*1000)
            return $ Node 0 ( k1*sqrt pn)  (Sprinkler (Just (d,k1)) (Just dm) area densidadeMin)


diffElement s@(RamalElement l) n = minFlow $ do
  let vinit = vNode n
      pinit = pNode n
  (Node pinit vlast res) <- solveRamal s n
  return (RamalNode 0 vlast  res )

diffElement i j = error ("No match for addElement " <> show i <> "   " <> show j)
nodePlus n1 (Node p v e) = Node (pNode n1 + p)  (vNode n1 + v ) e
nodePlus n1 (RamalNode p v e) = RamalNode (pNode n1 + p)  (vNode n1 + v ) e

nodeSub n1 (Node p v e) = Node (pNode n1 - p)  (vNode n1 - v ) e
nodeSub n1 (RamalNode p v e) = RamalNode (pNode n1 - p)  (vNode n1 - v ) e

addElement' j nodo = fmap (nodePlus nodo) (diffElement j nodo)
removeElement j nodo = fmap (nodeSub nodo) (diffElement j nodo)

testAddElement i j = addElement' i j == addElement i j

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

addElement s@(Sprinkler i  mdm area densidadeMin)  n = do
  let v = vNode n
      p = pNode n
  case  i of
       Just (d,k1) -> do
            let
                dm = maybe (minimumTubo (v + k1*sqrt p)) id mdm
                pn = p - pv v dm
            return $ Node p (v +  k1*sqrt pn) (Sprinkler i (Just dm) area densidadeMin)
       Nothing ->  do
            b@(d,k1) <- bicos
            let
                dm = maybe (minimumTubo (v + k1*sqrt p)) id mdm
                pn = p - pv v dm
            return $ Node p (v +  k1*sqrt pn)  (Sprinkler (Just (d,k1)) (Just dm) area densidadeMin)

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

addElement i j = error ("No match for addElement " <> show (i :: Element Double) <> "   " <> show (j :: Node (Element Double)))

-}


unNode :: Element a -> State ((Element a,Int),(Element a,Int)) Int
unNode e = do
  modify (\((_,i),j)-> ((e,i+1),j))
  snd . fst <$> get
unLink :: Element a -> State ((Element a,Int),(Element a,Int)) Int
unLink  e = do
  modify (\(i,(_,j))-> (i,(e,j+1)))
  snd . snd <$> get

stack (Left l) ((Sprinkler c _ i j):xs) = do
  el <- fst.snd <$> get
  let e = Sprinkler c (diametroE el) i j
  un <- unNode  e
  (i,r) <- stack (Right un) xs
  return $ (i,([(un,e)],[l un []]) <> r)
stack (Right i) l@(el:xs) = do
  un <- unLink (last l )
  stack (Left (\t xs -> (un,i,t,el:xs))) xs
stack (Left f ) (Joelho jdia  j k l :xs) = do
  eo <- fst.snd <$> get
  stack  (Left (\i xs -> f i (Joelho (fromMaybe (diametroE eo) (Just <$> jdia) ) j k l :xs))) xs
stack (Left f ) (el:xs) = do
  stack  (Left (\i xs -> f i (el:xs))) xs
stack (Left i) [] = do
  return (i,([],[]))
stack (Right i)  j  = error $ "no match" <>  show j <> " " <> show i


diametroEJ i = case diametroE i of
                    (Just i) -> i
                    Nothing -> error $ "sem diametro elemento " ++ show i
unrollNode (ln,oe) e@(Origem i) =  do
  (el ,l) <- stack (Right ln) (init i)
  ul <- (snd <$> get)
  (n,r) <- unrollNode   (swap ul) (last i)
  return $ (snd ul,([],[el n []]) <> l<> r)
unrollNode (ln,oe) e@(Te _ n i j ) = do
  un <- unNode e
  uljl  <-(snd .snd <$> get)
  (elj ,lj ) <- stack (Right un) (init j)
  ulj  <-(snd <$> get)
  (nj,rj) <- unrollNode  (swap ulj) (last j)
  ulil  <-(snd .snd <$> get)
  (eli ,li ) <- stack (Right un) (init i)
  uli  <-(snd <$> get)
  (ni,ri) <- unrollNode  (swap uli) (last i)

  let conf = case n of
       TeBranch -> TeeConfig [uljl +1 , ln ,ulil +1 ] 0.01 (diametroEJ oe) (max (diametroEJ oe ) $ max (diametroEJ (head i)) (diametroEJ (head j)))
       TeRunL -> TeeConfig [ln,uljl +1 ,ulil +1 ] 0.01 (diametroEJ (head j)) (max (diametroEJ (head j)) $max (diametroEJ (head i)) (diametroEJ oe))
       TeRunR -> TeeConfig [uljl +1 ,ulil +1,ln ] 0.01 (diametroEJ (head i)) (max (diametroEJ (head i)) $max (diametroEJ oe) (diametroEJ (head j)))

  return $ (un,([(un,Tee ( traceShow (oe ,(head i) ,(head j)) $ traceShowId $ conf 100)Table )],[elj nj [], eli ni []]) <> lj <> li  <> ri <> rj)
unrollNode (ln,oe) e@(Sprinkler _ _ _ _ ) = do
  un <- unNode e
  return $ (un,([(un,e)],[]))
unrollNode (ln,oe) e@(Open _) = do
  un <- unNode e
  return $ (un,([(un,e)],[]))
unrollNode (ln,oe) e@(Reservatorio _ ) = do
  un <- unNode e
  return $ (un,([(un,e)],[]))
unrollNode i j = error $ show i ++ show j


editDiametro v (Tubo Nothing b c )  = Tubo (Just v) b c
editDiametro v (Joelho Nothing b c d)  = Joelho (Just v) b c d
editDiametro v i = i

{-
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

          ssr = [tubo50 1.76,spo50,tubo50 3.57,spo50 ,tubo50 3.57,spo50,tubo50 3.57,spo50]
          ss21 = [tubo75 1.76  ,teLat75 ] ++ ssr
          ss22 = [tubo75 1.76 ,te75 ssr ([tubo 3.57 , teLat75] ++ ssr)]
          ss2 = [teLat75, teDireta75 , gaveta, tubo75 1.0 , te75 ss21 ss22 ]

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
          l10 = [tubo75  3.28 , te75 [tubo 0.9,sp] [tubo75 2.33 , cruz75 [tubo 6.40,sp] [tubo 4.6,sp] l11]]
          l11 = [tubo75 1.92 , te75 [tubo 0.9,sp] [tubo75 1.4, te75 [tubo 5.3,sp,tubo 4.2,sp] [tubo 3.5,sp,tubo 4.1 , sp]]]
          sp = Sprinkler (Just (11,5.8)) Nothing 11 4.1
          spo = Sprinkler (Just (11,5.8)) (Just 0.075) 12 6.1
          spo50 = Sprinkler (Just (11,5.8)) (Just 0.05) 12 6.1
          tubo d = Tubo Nothing d 100
          retencao = Joelho Nothing ("Valvula","Retencao","") DRight 100
          gaveta = Joelho Nothing ("Valvula","","Gaveta") DRight 100
          tubod di d = Tubo (Just di) d 100
          joelho d = Joelho Nothing ("Conexao","Joelho","90") d 100
          teLat d = Joelho Nothing ("Conexao","Te","Lateral") d 100
          teLatD d = Joelho (Just 0.075) ("Conexao","Te","Lateral") DRight 100
          teDireta  = Joelho Nothing ("Conexao","Te","Direta") DRight 100
          teDireta75  = Joelho (Just 0.075) ("Conexao","Te","Direta") DRight 100
          te i j = Te Nothing TeBranch i j
          ted d i j = Te (Just d) TeBranch i j
          opte i j = OptTe  TeBranch i j
          cruz i j k = te i [te j k ]
          tubo75 = tubod 0.075
          tubo50 = tubod 0.050
          te75   = ted 0.075
          teLat75 =  teLatD 0.075
          cruz75 i j k = te75 i [te75 j k ]
          renderNode1 name = mapM (\(i,l) -> when (not $ null l ) $  writeFile (name<> backed i <> ".iter" ) . (("\n ----------------- \n Iteration - " ++ show i ++ "\n ----------------- \n" ) ++ ).  L.intercalate ("\n\n") . fmap (\(l,v) -> "- Option " ++ show (l :: Int) ++ "\n" ++ v) . zip [0..]. fmap (fmap (\i -> if i == '.' then ',' else i ) . L.intercalate "\n") . fmap (fmap (showNode 0)) $ l)  . zip [0 :: Int ..]
   let  r1 = pressurePath (Origem r8 )
        s1 = pressurePath  (Origem ss2)
        req = [teDireta,gaveta,  OptTe TeBranch r8  l0]
        t21 = pressurePath (Origem l0)
        firstpass = last $ last r1
        succao2 = [tubod 0.1 1.6,joelho DRight, tubod 0.1 1.0,joelho DRight , tubod 0.1 1.6 , Gravidade (1.0),te [gaveta,tubod 0.1 0.4,Gravidade (0.4),Bocal (Just 0.1)] [gaveta,tubod  0.1 0.8 ,joelho DRight ,tubod 0.1 0.4,Gravidade (0.4),Bocal (Just 0.1)]]
        -- succao = [tubod 0.1 1.6,gaveta,retencao,joelho DRight, tubod 0.1 1.0,joelho DRight , tubod 0.1 1.6 , tubo 0.8 ,joelho DRight ,tubo 0.4]
        recalque = [tubod 0.1 1.2 ,Gravidade (-1.2) , retencao , gaveta,joelho DRight , tubo 0.5, teLat DRight ,tubo 3 ,joelho DRight , tubo 4, joelho DRight , tubo 3.2, joelho DRight , tubod 0.1 5.43,Gravidade 5.43]
        bombaTipo12 =  Reservatorio 30 Nothing $ Bomba Nothing  (Curva bombaSuccaoFrontal ) (recalque ++  [ subsolo] ) succao2
        tipo12=  req
        tipo1=  Origem ([tubod 0.1 (2.89*11),Gravidade (2.89*11) ] ++ l0 )
        subsolo=  OptTe  TeBranch req  (replicate 10 teDireta ++ [ tubod 0.1 (2.89*11),Gravidade (2.89*11) ,opte l0 (replicate 2 teDireta ++ [ tubod 0.1 (3*2.89),Gravidade (3*2.89)  ] ++ ss2)])
   --renderNode1 "ppath" [(pressurePath $ Origem ss2)]
   --renderNode1 "tipo-12" [pressurePath tipo12]
   --renderNode1 "tipo-1" [pressurePath tipo1]
   --renderNode1  "subsolo" $ [pressurePath $ subsolo]
   -- mapM (\(i,l) -> writeFile ("tipo1" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath tipo1
   -- mapM (\(i,l) -> writeFile ("tipo12" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath tipo12
   -- mapM (\(i,l) -> writeFile ("subsolo" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath subsolo
   -- writeExcelCsv "himalaia" $  backPressurePath (Origem succao2) (Node 0 (-1300) (joelho DRight))
   writeExcelCsv preheader "himalaia" $  pressurePath  bombaTipo12
   --mapM (\(i,l) -> writeFile ("bombaTipo1" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath bombaTipo1
   -- mapM (\(i,l) -> writeFile ("bombaSubsolo" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  pressurePath bombaSubsolo
   {-renderNode1 "t21" t21
   renderNode1 "r1" r1
   -- renderNode1  "r" $  result1N req
   -- mapM (\(i,l) -> writeFile ("andar_12" ++ show i ++ ".csv" ) (header ++ l) >> print i) $ zip [0..] $ (fmap (fmap (\i -> if i == '.' then ',' else i ). nodeLists  ) )$  last $ result1N req
-}
-}

-- main  = twopass

backed i = backedzero i ++ show i
backedzero i
  | i <10 = "000"
  | i <100 = "00"
  | i <1000 = "0"

showJust f (Just i ) = f i
showJust _ Nothing = ""



i <|>  j = i ++ ";" ++ j




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

bomb =fmap (\(p,v)-> (p,v*1000.0/60)) [(250,60),(250,70),(250,80),(250,90),(300,60),(300,70),(300,80),(300,90),(350,60),(350,70),(350,80),(350,90),(400,60),(400 ,70),(400,80),(400,90),(450,60),(450,70), (450,80),(450,90)]

formatFloatN numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

