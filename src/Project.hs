{-# LANGUAGE RecursiveDo,OverloadedStrings ,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Project where

import Grid
import qualified Data.Set as S
import Search
import GHC.Stack
import Debug.Trace
import Control.Applicative.Lift
import Force (bendIter)
import Data.Graph
import Element
import Domains

import qualified Data.Array as A
import Backend.DXF
import DXF
import Polygon
import Point2 hiding ((<*>))

import Data.Functor.Compose
import Data.Functor.Identity
import Hydraulic
import Rotation.SO3 hiding (rotM)
import System.Process
import Control.Monad
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Position
import Data.Maybe
import Sprinkler
import Tee
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Mecha

import Control.Lens

import Diagrams.Prelude hiding (end,Ambient)


path i h t  l = (i,h,t,  l)

jd dir d = Joelho dir (TabelaPerda (Circular d) ("Conexao","Joelho","90")  100)

-- makeIter :: (Coord f (V3 Double),PreSys f ,Show (f Double),Functor f, Functor (NodeDomain f ) ,Functor (LinkDomain f), Floating a )=> Int -> Int -> Grid f Double -> Iteration f a
makeIter i j g = initIter (realToFrac  <$>  fst (upgradeGrid i j g))


data ProjectHeader
  = ProjectHeader { projectName :: String
    , endereco :: String
    , projectOwner :: String
    , authorInfo :: Author
    , baseFile :: String
    , regionInfo :: [Region]
    }

data Region
  = Region
    { regionName :: String
    , regionFile :: String
    , regionView :: [(String,String)]
    , regionBoundary :: [V2  Double]
    , regionRisk :: RegionRisk
    }
data RiskClass
   = Light
   | OrdinaryI
   | OrdinaryII
   | ExtraOrdinaryI
   | ExtraOrdinaryII
   deriving (Eq,Show,Ord)
data ComodityClass
   = ComodityI
   | ComodityII
   | ComodityIII
   deriving(Eq,Show,Ord)

data RegionRisk
  = Standard
  { stdrisk ::  RiskClass
  }
  | MiscelaneousStorage
  { stdrisk :: RiskClass
  , storageHeight :: Double
  }
  | HighpilledStorage
  { commodityClass :: ComodityClass
  , storageHeight :: Double
  }deriving(Eq,Show,Ord)

interp ((a1,a2),(b1,b2)) =   (c,a1 - b1*c)
    where
      c = (a2 - a1)/(b2 - b1)

eval x ordinaryII = case interp ordinaryII of
           (a,c) ->  a*x + c


ordinaryI = ((6.1,8.1), (372,139))
ordinaryII = ((6.1,8.1), (372,139))
light = ((6.1,8.1), (372,139))

stdCurves OrdinaryII = ordinaryII
stdCurves OrdinaryI = ordinaryI
stdCurves Light = light

comodityCurves  _ = undefined

regionRiskDensity (Region _ _ _ b (Standard i ))
  = eval   (abs $ Polygon.area (convPoly b)) (stdCurves i)
regionRiskDensity (Region _ _ _ b (MiscelaneousStorage i h ))
  | h > 3.7   = errorWithStackTrace "altura maior que 3.7"
  | otherwise =  eval (abs $ Polygon.area (convPoly b)) (stdCurves i)
regionRiskDensity (Region _ _ _ b (HighpilledStorage i h )) = errorWithStackTrace "not implemented"
  -- | h >= 3.7   = errorWithStackTrace "altura maior que 3.7"
  -- | otherwise =  eval (abs $ Polygon.area (convPoly b)) (stdCurves i)

data Author
  = Author { authorName :: String
    , creaNumber :: String
    , formation :: String
    , empresa :: String
    }



rightC d = d
right90  = rightC $ 1/4
right45  = rightC $ 1/8
leftC d = -d
left90   = leftC $ 1/4
left45   = leftC $ 1/8


end = [tubod 0.025 0.01,Open 0]
tee b i j = Te  Nothing b i j
cruz i j k = tee TeRunL [Atom (tubod 0.05 0.01),tee TeRunR  k j ] i
tubod  d l  = Tubo (Circular d) l 100

pj90 e = (TabelaPerda (Circular e) ("Conexao","Joelho","90") 100)
pj45 e = (TabelaPerda  (Circular e) ("Conexao","Joelho","45") 100)

joelhoR  e = Joelho right90  (pj90 e)
joelho  = joelhoR
joelhoL  e = Joelho left90 (pj90 e)
joelho45  = joelho45R
joelho45R e  = Joelho right45  (pj45 e)
joelho45L e = Joelho left45 (pj45 e)


-- testResistor :: SR.Expr

displayBended (header,model) = do
  T.writeFile (header <> "-temp.scad") $ openSCAD ({- Mecha.color (1,0,0,0.2) (drawIter  model) <>  -} drawIter (bendIter model))
  callCommand $ "mv " <> (header <> "-temp.scad") <>  "  " <> (header <> ".scad")

displaySolve (header,model) = do
  T.writeFile (header <> "-temp.scad") $ openSCAD (drawIter  model )
  callCommand $ "mv " <> (header <> "-temp.scad") <>  "  " <> (header <> ".scad")



displayModel (header,model) = do
  T.writeFile (header <> "-temp.scad") $openSCAD (drawGrid $ model )
  callCommand $ "mv " <> (header <> "-temp.scad") <>  "  " <> (header <> ".scad")

filterBounded poly grid = prune (grid {nodesFlow =spks}) tar
  where
    pcon = PolygonCCW $ fmap (\(V2 a b) -> Point2 (a,b)) poly
    spks = fmap (\(i,v) ->
      let
        (V3 x y _,_) = var i (M.fromList (shead grid))
      in (i,if Polygon.contains pcon (Point2 (x,y))   then v else if isSprinkler v then Open 0 else v )
        ) (nodesFlow grid)

    tar = fst <$> filter (Polygon.contains pcon. (\(V3 x y z) -> Point2 (x,y)) . fst . snd) (shead grid)

prune g t = g {nodesFlow = out1nodes   <> nn  , links = out1links <> nt }
    where
      l = concat $ (\(h,t,v)-> [(h,t)] <> (if L.any isValvulaGoverno v then [] else [(t,h)])) . snd <$> links g
      r = concat $ (\(h,t,v)->  (if L.any isValvulaGoverno v then [] else [(h,t)] <>[(t,h)])) . snd <$> links g
      reach = S.fromList $  concat $ fmap (\(i,j) -> [i,j]) $ concat $ concat $ fmap (flip (connected 0) graph) t
      graph = L.nub <$> buildG (0,length l) (L.sort rl)
      reachS =  S.fromList $ (t <>  reachable  (buildG (0,length l) l) (head t))
      rl = filter (\(h,t) -> S.member h reachP || S.member t reachP) l
      reachP = reachS <> reachRoot

      reachRoot = S.fromList $ (reachable  (buildG (0,length l) r) 0)
      -- Output reachable nodes
      out1nodes = filter (\(i,_)-> S.member i reach) (nodesFlow g)
      -- Output reachable links
      out1links = filter (\(l,(h,t,_)) -> S.member h reach && S.member t reach ) (links g)
      -- Output reachable set
      out1S = S.fromList $ fst <$> out1nodes

      -- Extra nodes to close the pruned the circuit
      (nt,nn) = unzip $ generate <$>  openN
        where
          openN = filter (\(l,(h,t,v)) -> (S.member h out1S||  S.member t out1S)&& not (S.member t out1S&& S.member h out1S)) (links g)
          generate  (l,(h,t,v))
            | S.member h out1S= ((l,(h, t, [tubod 0.025 0.1])), (t,Open 0))
            | S.member t out1S= ((l,(h, t, [tubod 0.025 0.1])), (h,Open 0))

renderModel regionRisk range (header,model) = do
  let fname  = baseFile header
      dxfname = fname <> ".DXF"
  Right f <- readDXF dxfname
  let calc_bounds  = filter ((== "calc_area").layer. eref) $ entities f
      projBounds (Entity n o (LWPOLYLINE _ _ _ polygon _ _ )) = fmap fst polygon
      regions (i,b) =  Region name (baseFile header  <> "-" <> name ) [] ( b)  (regionRisk !! i)
        where label =  L.find (\(Entity  _ _ (TEXT (V3 x y _)  _ _ _ _)) -> Polygon.contains  (PolygonCW $ fmap (\(V2 i j) -> Point2 (i,j)) b) (Point2 (x,y)))$  filter ((== "area_label").layer. eref) $ entities f
              name = maybe (show i) (\(Entity _ _ (TEXT _ _ l   _ _)) -> l) label
      modelg = fst $ upgradeGrid 0 1 $ model
  drawSCAD (baseFile header) (baseFile header) modelg
  renderDXF  (baseFile header) fname  (drawGrid  modelg)
  mapM (\r -> do
     solveModel (header , initIter (fst $ upgradeGrid 0 1 $ filterBounded (regionBoundary r)  $ modelg ) $ defAmbient,r))  (  regions <$> filter ((`elem` range ). fst) (zip [0..] (projBounds <$> calc_bounds)))


solveModel (header ,model,region ) = do
  let fname  = regionFile region
  let
       iter = solveIter (fmap realToFrac model ) (jacobianEqNodeHeadGrid (fmap realToFrac $ environment model) )
  reportIter header region 0 iter
  print $ "renderReport: " <> (fname <> ".csv")
  let sofficec = "soffice --convert-to xls --infilter=csv:\"Text - txt - csv (StarCalc)\":59/44,34,0,1,,1033,true,true  " <> fname <> ".csv"
  putStrLn sofficec
  callCommand sofficec
  print $ "renderReport: " <> fname <> ".xls"
  drawSCAD (baseFile header) fname (grid iter)

drawSCAD base fname  grid = do
  let scadFile = openSCAD (drawGrid $ grid ) <> "import(\"" <> T.pack base <> ".DXF\");"
  T.writeFile (fname <> "-temp.scad") scadFile
  let movefile = "mv " <> (fname <> "-temp.scad") <>  "  " <> (fname <> ".scad")
  callCommand movefile
  print $ "renderSCAD: " <> (fname <> ".scad")
  {-mapM (\v -> do
      let command  = "openscad -o " <> fname <> "-" <> fst v<> ".png " <> fname <> ".scad  " <> snd v
      putStrLn command
      callCommand  command) (regionView $ region )-}



data CyclePoint a b
  = CloseLink a
  | BranchLink b
  deriving(Eq,Show,Ord)




upgradeGrid :: (Show (f Double) , Coord f (V3 Double)) => Int -> Int -> Grid f Double -> (Grid  f Double,Errors [(Int,Int,String,Double)] [(Int,Int,(V3 Double ,SO3 Double))])
upgradeGrid ni li a = (a {shead = M.toList nodesPos, linksPosition = M.toList linksPos},err)
  where
    (err,(nodesPos,linksPos)) =  runState (do
          modify (<> (M.singleton ni (0,SO3 $ rotM 0), mempty))
          let
            niel = var ni nmap
            nels = fmap snd $ thisElement (fst niel) (snd niel)
            oi = var li nels
          i <- locateGrid lmap nmap ni oi  li (Left $ var li lmap )
          j <- mapM (\(inx,ie) -> locateGrid lmap nmap ni ie inx (Left $ var inx lmap) ) (filter ((/=li).fst) $ M.toList nels)
          return $ mappend <$> i <*> (foldr (liftA2 mappend ) (pure []) j))  (mempty,mempty)
    lmap = M.fromList (links a)
    nmap = M.fromList (findNodesLinks a $   (nodesFlow a) )

recurse render ni r@(Right l@(h,t,e)) = do
  lift $ modify (<> (S.empty,S.singleton ni))
  i <- fst <$> lift  get
  linkmap <- fst <$> ask
  let nexts = S.toList $ S.difference (S.fromList [h,t]) i
  ti <- mapM (\i -> recurse render i . Left . flip var linkmap $ i ) nexts
  return $ render ni r  : concat ti
recurse render ni r@(Left n@((lks,e))) = do
  lift $ modify (<>(S.singleton ni,S.empty))
  s <- snd <$> lift  get
  nodemap <- snd <$> ask
  let nexts = S.toList $ S.difference (S.fromList lks)  s
  ti <- mapM (\i -> recurse render i . Right . flip var nodemap $ i ) nexts
  return $ render ni r : concat ti

sf2 = show -- formatFloatN 2
sf3 = show -- formatFloatN 3

convPoly poly =   PolygonCCW $ fmap (\(V2 a b) -> Point2 (a,b)) poly

reportIter :: ProjectHeader -> Region -> Int -> Iteration Element Double -> IO ()
reportIter header rinfo i iter@(Iteration f h e a)  = do
    let name = regionFile $ rinfo
    writeFile (name <> ".csv")  $(headerCalculo <> "\n\n" <> "Relatório dos Nós" <> "\n\n" <>( L.intercalate "\n"    $ (L.intercalate ","  nodeHeader :) (evalState (runReaderT (do
           nodemap  <- fst <$> ask
           recurse (\ni -> either (nmap ni) (lmap ni)) i (Left $ fromJustE "no node" $ M.lookup i nodemap )) (M.fromList $  findNodesLinks a (nodesFlow a) ,M.fromList $ links a )) (S.empty,S.empty))) <>
            "\n\n" <> "Relatório dos Links" <> "\n\n" <> L.intercalate "," linkHeader <>"\n" <> (L.intercalate "\n" $ lsmap <$> (links a)))
  where
    residual = printResidual iter (jacobianEqNodeHeadGrid defAmbient)
    reportEnviroment (Ambient f i j ) =
       L.intercalate "\n" $ L.intercalate "," <$> [["Condições Ambiente"],["Fluído",show $ fluidName f],["Viscosidade Dinâmica", show $ viscosity f],["Densidade Fluído",show $ density f],["Gravidade",sf3 i],["Densidade Ar",sf3 j]]
    reportGrelha =
      let t  = L.filter (isGrelha.snd) (nodesFlow a)
          title = ["Grelhas"]
          header = ["Id","Vazao","Velocidade","Velocidade Min"]
          headerUnit =  ["", "L/min","m/s","m/s"]
          proj (i,Grelha h w a m ) =  [show i ,sf2 vazao  , sf2 (vazao/a/1000/60), sf2 m ]
            where vazao = head $ (\i -> fromJustE "no flow for node 1 " $  join $ M.lookup i fm)<$>  (fst lks)
                  lks = var i nlmap

      in [L.intercalate "\n" $ L.intercalate "," <$> (title : header : headerUnit : (proj <$> t))]
    reportResev =
      let t  = L.filter (isReservoir .snd) (nodesFlow a)
          proj (i,Reservatorio tempo) = "Reservatório\n" <> "Volume," <> sf2 (vazao * tempo) <> ",L\n" <> "Tempo de Duração," <> sf2 tempo<> ",min"
            where vazao = head $ (\i -> fromJustE "no flow for node 1 " $  join $ M.lookup i fm)<$>  (fst lks)
                  lks = var i nlmap

      in (proj <$> t)
    nlmap = M.fromList (findNodesLinks a $   (nodesFlow a) )
    res he residual =
       L.intercalate "\n" $ L.intercalate ";" <$> [[he],
        ["Máximo", show (L.maximum $ abs <$> residual)],
        ["Mínimo",show (L.minimum $ abs <$> residual)],
        ["Norma", show (sqrt $ sum $ fmap (^2) residual)]]
    resc= res "Resíduo Node Head" (L.take (length (flows iter)) residual)
    resnh = res "Resíduo Continuity" (L.drop (length (flows iter)) residual)
    residuos = "Dados Solução\n\n" <> resnh <> "\n"  <>resc
    vazao = fromJustE "no flow for node 1 " $ join $ M.lookup 1 fm
    display (i,l ) = i <> "\n" <> L.intercalate "\n" (L.intercalate "," <$> l)
    bomba :: Maybe (String ,[[String]])
    bomba = fmap result $  join $ (\(i,(_,_,l)) -> (i,) <$> L.find isBomba l)<$> L.find (\(_,(_,_,l)) -> L.any isBomba l ) (links a)
      where result (ix,bomba@(Bomba (pn,vn) _ )) = ("Bomba", [["Pressão Nominal",  sf2 pn , "kpa"] ,["Vazão Nominal", sf2 vn , "L/min",sf2 $ vn*60/1000,"m³/h"] ,["Potência", maybe "-" (sf2.potenciaB) dbomba ,"cv"], ["Rotação", maybe "-" (sf2.rotacaoB) dbomba  ,"rpm"],["Vazão Operação",sf2 vazao , "L/min", sf2 $vazao* 60/1000,"m³/h"] ,["Pressão Operação", sf2 pressao , "kpa"],["Potência Necessária" , sf2 $ power, "kW",sf2 $ power*kwtocv,"cv"],["Eficiência Est.",sf2 eficiencia,"W/W"], ["Potência Est." , sf2 $ power/eficiencia, "kW",sf2 $ power/eficiencia*kwtocv,"cv"]])
              where
                power = pressao * vazao/60000
                eficiencia = 0.55
                kwtocv = 1.36
                dbomba = M.lookup (traceShowId (pn/10,fromIntegral  $ round$ vn*3/50)) dadosBomba
                pressao = abs $ pipeElement (environment iter ) vazao bomba
                vazao = fromJustE "no flow for node 1 " $ join $ M.lookup ix fm

    sprinklers =  fmap (\(i,e) -> (i,fromJustE "no sprinkler" $ join $ M.lookup i  hm ,e)) $  L.filter (isSprinkler . snd) (nodesFlow a)
    addTee k = maybeToList (M.lookup k nodeLosses)
    sflow = signedFlow a  (justError "no flow " .runIdentity .getCompose <$> M.fromList f)

    projectHeader =  L.intercalate "\n" ["Projeto," <> projectName header   ,"Proprietário," <> projectOwner header , "Endereço," <> endereco header, formation ainfo <> "," <> authorName ainfo , "Crea," <> creaNumber ainfo , "Empresa," <>   empresa ainfo , "Região Cálculo," <> regionName rinfo ]
        where ainfo = authorInfo header
    headerCalculo = projectHeader <> "\n\n" <> "Dados do projeto" <> "\n\n" <> L.intercalate "\n\n"
            ( [ reportEnviroment (environment iter)] <> maybeToList (display <$>bomba )<> reportGrelha <> reportResev <> [ unlines $ L.intercalate "," <$> regionReport rinfo ]) <>  "\n\n" <>residuos

    regionReport r =  [ ["Nome da Região",regionName  r] ,["Classe de Risco" , riskName $ regionRisk r],["Área da Região", sf2 area,"m²"] ,["Densidade de Água",sf2 density,"L/min/m²"], ["Vazão Mínima Área", sf2 minimalFlow ,"L/min"], ["Quantidade de Bicos", show nspk ,"un"] , ["Id","Pressão","Vazão","Vazão Min","Área" ],["","kpa","L/min","L/min","m²"]]<> spkReport
        where riskName (Standard i) =  show i
              riskName (MiscelaneousStorage i _) = show i
              nspk = L.length sprinklers
              area = abs $ Polygon.area (convPoly $ regionBoundary r)
              density = regionRiskDensity r
              minimalFlow = density * area
              spkReport = (\(ix,p,e@(Sprinkler ((d,k )) (dl) f a ))-> [show ix , sf2 p ,   sf2 $ genFlow p e , sf2 $ coverageArea f * density ,sf2 $ coverageArea f]  ) <$>    sprinklers
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t conf ) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTeeEl (fluido (environment iter))conf (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow a


    nmap = (\ni n@(_,e) -> L.intercalate "," $ ["N-"<> show ni,formatFloatN 4 $ maybe 0 id $p ni , formatFloatN 4 $h  ni,"",""] ++  expandNode (p ni) e )
        where p ni =  join $ varM ni hm
              h ni =  fst ( (fromJustE $"no position pressure for node " <> show ni) (varM ni  pm)) ^. _z
    lmap = (\ni n@(h,t,e) ->  L.intercalate "," ["T-"<> show h <>  "-" <> show t ,"","",formatFloatN 2 $  maybe 0 id (abs <$> p ni) ,formatFloatN 2  $ abs (pr h - pr t),"Trecho"])
        where pr ni =  maybe 0 id (join $ varM ni hm)
              p ni =  join $ varM ni fm
    lsmap n@(ni,(h,t,e)) = L.intercalate "\n" $ fmap (L.intercalate ",") $ (mainlink:) $ zipWith3 (expandLink (lname <>  "-") (p ni) ) [0..] dgrav els
        where p ni =  join $ varM ni fm
              mainlink = [lname  , "", sf2 $ gravp  ,sf3 $ negate $ fromMaybe 0 (join $ varM h hm ) - fromMaybe 0 (join $ varM t hm) + gravDelta (var h pm ) (var t pm) ,"", ""]
                where gravp =gravDelta (var h pm ) (var t pm)
              lname = "T-"<> show h <>  "-" <> show t
              grav =  (if isJust (M.lookup (h,ni) nodeLosses) then(var h pm :)else id ) (var ni lpos  <> [var t pm,var t pm])
              dgrav = zipWith gravDelta grav (drop 1 grav)
              gravDelta i j = gravityEffect (environment iter) (i ^. _1._z  - j ^. _1._z)
              els = addTee (h,ni) <> e <> addTee (t ,ni)
    fm = runIdentity . getCompose <$> M.fromList f
    hm = runIdentity . getCompose <$> M.fromList h
    pm = M.fromList (shead a )
    lpos = M.fromList (linksPosition a )
    nodeHeader = ["ID","Pressão (kpa)","Altura (m)","Vazão (L/min)","Perda (kpa)","Elemento"]
    expandNode (Just p) (Sprinkler ((d,k)) (dl) f a ) = ["Sprinkler"]
    expandNode _ (Reservatorio  _ )  = ["Reservatorio"]
    expandNode _ (Grelha _ _ _ _ )  = ["Grelha"]
    expandNode _ (Tee (TeeConfig i r c) _ )  = [show c]
    expandNode _ (Open 0 )  = ["Tampa"]
    expandNode _ (Open i )  = ["Hidrante"]
    expandNode _ i = []
    linkHeader = ["SubTrecho","Diametro (m)","Gravidade(kpa)", "Atrito(kpa)","Elemento","Comprimento (m)"]
    expandLink st (Just f) i dg t@(Tubo s dl  _) = [st <> show i ,  secao s , sf2 dg ,sf2$   Element.ktubo t*(abs f)**1.85,"Tubo " , sf3 dl ]
      where
        secao (Rectangular h w ) = sf3 h <> "x" <> sf3 w
        secao (Circular d)  = sf3 d
    expandLink st (Just f) i dg t@(Turn   _) = [st <> show i ,   "" ,sf2 dg , "","Turn" , "" ]
    expandLink st (Just f) i dg b@(Bomba (d,_)  dl  ) = [st <> show i, "",sf2 dg , sf2 $ pipeElement (environment iter) f b,"Bomba"]
    expandLink st (Just f) i dg j@(Joelho _(TabelaPerda (d)  (_,_,c)  _ ) )  = [st <> show i , sf3 d,sf2 dg , sf2 $Element.ktubo j*(abs f)**1.85,"Joelho " <> c]
    expandLink st (Just f) i dg j@(Perda (TabelaPerda (d)  (s,m,c)   _) )  = [st <> show i , sf3 d,sf2 dg , sf2 $Tee.ktubo j (abs f/1000/60),s <> m <> c]



findNodesLinks grid = fmap (\(i,n) -> (i,(var i nodeMapSet,n)))
    where nodeMapSet = fmap L.nub $ M.fromListWith mappend $ concat $ (\(l,(h,t,_)) -> [(h,[l ]),(t,[l ])]) <$> links grid
