{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Project where

import Backend.Mecha hiding (Polygon (..), Vertex)
import Control.Applicative
import Numeric
import Equation
import Data.Functor.Classes
import MultiLinear.Class
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Array as A
import Data.Ext
import qualified Data.Foldable as F
import Data.Geometry.Point
import Data.Graph
import Data.Geometry.Polygon as Geom
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text.IO as T
import Domains
import Element
import Grid
import Linear.Vector
import Hydraulic
import Linear.V2
import Linear.V3
import Position
import Search
import System.Process

path i h t l = (i, h, t, l)

jd dir d = Joelho dir (TabelaPerda (Circular d) ("Conexao", "Joelho", "90") 100)

formatFloatN numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

data ProjectHeader = ProjectHeader
  { projectName :: String,
    endereco :: String,
    projectOwner :: String,
    authorInfo :: Author,
    baseFile :: String,
    regionInfo :: [Region]
  }

interp ((a1, a2), (b1, b2)) = (c, a1 - b1 * c)
  where
    c = (a2 - a1) / (b2 - b1)

eval x ordinaryII = case interp ordinaryII of
  (a, c) -> a * x + c

ordinaryI = ((6.1, 8.1), (372, 139))

ordinaryII = ((6.1, 8.1), (372, 139))

light = ((6.1, 8.1), (372, 139))

stdCurves OrdinaryII = ordinaryII
stdCurves OrdinaryI = ordinaryI
stdCurves Light = light

comodityCurves _ = undefined

regionRiskDensity (Region _ _ _ (b, _) (Standard i)) =
  eval (abs $ area (convPoly b)) (stdCurves i)
regionRiskDensity (Region _ _ _ (b, _) (MiscelaneousStorage i h))
  | h > 3.7 = error "altura maior que 3.7"
  | otherwise = eval (abs $ area (convPoly b)) (stdCurves i)
regionRiskDensity (Region _ _ _ (b, _) (HighpilledStorage i h)) = error "not implemented"

data Author = Author
  { authorName :: String,
    creaNumber :: String,
    formation :: String,
    empresa :: String
  }

rightC d = d

right90 = rightC $ 1 / 4

right45 = rightC $ 1 / 8

leftC d = - d

left90 = leftC $ 1 / 4

left45 = leftC $ 1 / 8

end = [tubod 0.025 0.01, Open 0]

tee b i j = Te Nothing b i j

cruz i j k = tee TeRunL [Atom (tubod 0.05 0.01), tee TeRunR k j] i

tubod d l = Tubo (Circular d) l 100

pj90 e = TabelaPerda (Circular e) ("Conexao", "Joelho", "90") 100

pj45 e = TabelaPerda (Circular e) ("Conexao", "Joelho", "45") 100

joelhoR e = Joelho right90 (pj90 e)

joelho = joelhoR

joelhoL e = Joelho left90 (pj90 e)

joelho45 = joelho45R

joelho45R e = Joelho right45 (pj45 e)

joelho45L e = Joelho left45 (pj45 e)

-- testResistor :: SR.Expr

sprinklerRegionActivate ids grid = (prune compactGrid ids,pos)
  where
    pos = (fst $ upgradeGrid 0 1 compactGrid )
    compactGrid = (compactNodes $ grid {nodes = spks})
    spks = map (\(i, v) -> (i, if i `elem` ids then v else if isSprinkler v then Open 0 else v)) (nodes grid)

filterBounded :: (RealFloat a, Show a) => ([V2 a], b) -> CoordinateGrid V3 a -> Grid Element a -> (Grid Element a, CoordinateGrid V3 a)
filterBounded (poly, _) coord grid = sprinklerRegionActivate tar grid
  where
    pcon = convPoly poly
    spks =
      fmap
        ( \(i, v) ->
            let (V3 x y _, _) = var i (M.fromList (nodesPosition coord))
             in (i, if insidePolygon (Point2 x y) pcon then v else if isSprinkler v then Open 0 else v)
        )
        (nodes grid)
    tar = fst <$> filter (flip insidePolygon pcon . (\(V3 x y z) -> Point2 x y) . fst . snd) (nodesPosition coord)

gridSize g = (length (nodes g), length (links g))

-- Substitute old indexes
recursiveSubs m l = join (recursiveSubs m <$> lk) <|> lk
  where
    lk = M.lookup l m

-- Compact links with open nodes
compactNodes g = res
  where
    res = g {nodes = fmap (substitute subs) <$> filter (not . flip S.member removed . fst) (nodes g), links = fmap (\(l, h, t, e) -> (l, (h, t, e))) (F.toList lks)}
    substitute subs (Tee (TeeConfig a b c) t) = (Tee (TeeConfig ((\i -> fromMaybe i . recursiveSubs subs $ i) <$> a) b c) t)
    substitute subs i = i
    (_, _, subs, removed, lks) = execState (edit (0, 2)) (S.singleton 0, L.maximum $ fst <$> links g, M.empty, S.empty, M.fromList $ fmap (\(l, (h, t, e)) -> ((h, t), (l, h, t, e))) (links g))
    edit ini@(a, i) = do
      (visited, n, subs, removed, st) <- get
      put (visited <> S.singleton i, n, subs, removed, st)
      let next = L.delete a (lg A.! i)
          isOpen (Open _) = True
          isOpen _ = False
          lidx (ix, _, _, _) = ix
          lrev (ix, a, b, e) = (ix, b, a, e)
          el = head next
          cond = length next == 1 && isOpen (var i (M.fromList (nodes g)))
      if cond
        then do
          (visited, n, subs, removed, st) <- get
          if isJust (M.lookup (i, el) st)
            then do
              let st1 = M.delete (i, el) $ M.delete (a, i) $ st
                  st2 = M.insert (a, el) (mergeEl (lookFlip (a, i)) (lookFlip (i, el))) st1
                  lookFlip ix = justError ("no lookFlip " <> show (ix, ini, next)) $ M.lookup ix st
                  mergeEl (_, h, _, e) (_, _, t, a) = (n + 1, h, t, e <> a)
              put (visited, n + 1, M.insert (lidx $ lookFlip (a, i)) (n + 1) $ M.insert (lidx $ lookFlip (i, el)) (n + 1) subs, S.insert i removed, st2)
            else do
              put (visited, n, subs, removed, st)
              return ()
          when (not $ S.member el visited) $
            edit (a, el)
        else do
          mapM
            ( \j -> do
                (_, _, _, _, st) <- get
                when (not (S.member j visited) && isJust (M.lookup (i, j) st)) $
                  edit (i, j)
            )
            next
          return ()
    lg = buildG (0, maximum (fst <$> nodes g)) $ fmap (\(l, (h, t, e)) -> (h, t)) (links g)

-- Search incrementally for new connected nodes
process :: Vertex -> Vertex -> ([Edge], Set Vertex) -> ([(Vertex, Vertex)], Set Vertex)
process maxn n (b, o) = (filter (\(i, b) -> not (S.member i sc && S.member b sc)) b, sc)
  where
    nc = concat $ fmap (\(i, j) -> [i, j]) $ concat $ concat $ (\i -> connected i n (buildG (0, maxn) b)) <$> S.toList o
    sc = o <> S.fromList nc

processG :: Vertex -> Vertex -> ([Edge], Set Vertex) -> ([(Vertex, Vertex)], Set Vertex)
processG maxn n (b, o) = (filter (\(i, b) -> not (S.member i sc && S.member b sc)) b, sc)
  where
    nc = reachable (buildG (0, maxn) b) n
    sc = o <> S.fromList nc

-- Filter connected
prune :: Fractional a => Grid Element a -> [Int] -> Grid Element a
prune g nodeSet = g {nodes = out1nodes <> nn, links = out1links <> nt}
  where
    l = concat $ (\(h, t, v) -> [(h, t)] <> (if L.any isValvulaGoverno v then [] else [(t, h)])) . snd <$> links g
    r = concat $ (\(h, t, v) -> (if L.any isValvulaGoverno v then [] else [(h, t), (t, h)])) . snd <$> links g
    reach = snd $ foldr (process maxn) (rl, S.singleton 0) nodeSet
    maxn = maximum (fst <$> nodes g)
    reachS = S.fromList nodeSet <> snd (foldr (processG maxn) (l, S.empty) nodeSet)
    rl = filter (\(h, t) -> S.member h reachP || S.member t reachP) l
    reachP = reachS <> reachRoot
    reachRoot = S.fromList $ (reachable (buildG (0, maxn) r) 0)
    -- Output reachable nodes
    out1nodes = filter (\(i, _) -> S.member i reach) (nodes g)
    -- Output reachable links
    out1links = filter (\(l, (h, t, _)) -> S.member h reach && S.member t reach) (links g)
    -- Output reachable set
    out1S = S.fromList $ fst <$> out1nodes
    -- Extra nodes to close the pruned the circuit
    (nt, nn) = unzip $ generate <$> openN
      where
        openN = filter (\(l, (h, t, v)) -> (S.member h out1S || S.member t out1S) && not (S.member t out1S && S.member h out1S)) (links g)
        generate (l, (h, t, v))
          | S.member h out1S = ((l, (h, t, [tubod 0.025 0.1])), (t, Open 0))
          | S.member t out1S = ((l, (h, t, [tubod 0.025 0.1])), (h, Open 0))

renderModel :: Ambient Double -> p1 -> p2 -> (ProjectHeader, Grid Element Double) -> IO ()
renderModel env regionRisk range (header, model) = do
  let fname = baseFile header
      dxfname = fname <> ".DXF"
  -- Right f <- readDXF dxfname
  let -- calc_bounds  = filter ((== "calc_area").layer. eref) $ entities f
      --projBounds (Entity n o (LWPOLYLINE _ _ z polygon _ _ )) = (fromMaybe 0 z,fmap fst polygon)
      --regions (i,(z,b)) =  Region name (baseFile header  <> "-" <> name ) [] ( b,z)  (regionRisk !! i)
      --    where label =  L.find (\(Entity  _ _ (TEXT (V3 x y zt)  _ _ _ _)) -> Polygon.contains  (PolygonCW $ fmap (\(V2 i j) -> Point2 (i,j)) b) (Point2 (x,y)) && (zt < z + 1 || zt > z-1) )$  filter ((== "area_label").layer. eref) $ entities f
      --          name = maybe (show i) (\(Entity _ _ (TEXT _ _ l   _ _)) -> l) label
      modelg = fst $ upgradeGrid 0 1 $ model
  drawSCAD (baseFile header) (drawGrid modelg model)
  -- renderDXF  (baseFile header) fname  (drawGrid  modelg  )
  --mapM (\r -> do
  --   solveModel (header , initIter (filterBounded (regionBoundary r) modelg) env,r))  (regions <$> filter ((`elem` range ). fst) (zip [0..] (projBounds <$> calc_bounds)))
  let (newg,pos) = sprinklerRegionActivate [372 .. 378] model 
  solveModel (header, initIter  newg pos env, undefined)

solveModel :: Real a => (ProjectHeader, FIteration V3 V3 Identity Ambient Element a, Region) -> IO ()
solveModel (header, model, region) = do
  let fname = "regionName" -- regionFile region
      iter = solveIter (fmap realToFrac model) (jacobianEqNodeHeadGrid (fmap realToFrac $ environment model))
  printResidual iter (reportIter header region 0 (environment iter))
  print $ "renderReport: " <> (fname <> ".csv")
  let sofficec = "soffice --convert-to xls --infilter=csv:\"Text - txt - csv (StarCalc)\":59/44,34,0,1,,1033,true,true  " <> fname <> ".csv"
  putStrLn sofficec
  callCommand sofficec
  print $ "renderReport: " <> fname <> ".xls"
  drawSCAD fname (drawGrid (position iter) (grid iter)) -- <> drawRegion region )

displayModel :: (Show1 sys, Coord sys V3, Show (sys Double), Target sys Solid) => (FilePath, Grid sys Double) -> IO ()
displayModel (file, grid) = drawSCAD file (drawGrid (fst $ upgradeGrid 0 1 grid) grid)

drawSCAD :: FilePath -> Solid -> IO ()
drawSCAD fname grid = do
  let scadFile = openSCAD grid
  T.writeFile (fname <> "-temp.scad") scadFile
  let movefile = "mv " <> (fname <> "-temp.scad") <> "  " <> (fname <> ".scad")
  callCommand movefile
  print $ "renderSCAD: " <> (fname <> ".scad")

{-mapM (\v -> do
    let command  = "openscad -o " <> fname <> "-" <> fst v<> ".png " <> fname <> ".scad  " <> snd v
    putStrLn command
    callCommand  command) (regionView $ region )-}

sf2 = show -- formatFloatN 2

sf3 = show -- formatFloatN 3

convPoly :: [V2 a] -> Polygon Geom.Simple () a
convPoly poly = fromPoints ((\(V2 i j) -> Point2 i j :+ ()) <$> poly)

reportIter :: a ~ Double => ProjectHeader -> Region -> Int -> Ambient a -> CoordinateGrid V3 a -> Grid Element a -> M.Map Int (LinkDomain Element a) -> M.Map Int (NodeDomain Element a) -> IO ()
reportIter header rinfo i env p a f h = do
  let name = "regionName" -- regionFile $ rinfo
  writeFile
    (name <> ".csv")
    $( headerCalculo <> "\n\n" <> "Relatório dos Nós" <> "\n\n"
         <> ( L.intercalate "\n" $
                (L.intercalate "," nodeHeader :)
                  ( evalState
                      ( runReaderT
                          ( do
                              nodemap <- fst <$> ask
                              recurse (\ni -> either (nmap ni) (lmap ni)) i (Left $ var i nodemap)
                          )
                          (M.fromList $ findNodesLinks a (nodes a), M.fromList $ links a)
                      )
                      (S.empty, S.empty)
                  )
            )
         <> "\n\n"
         <> "Relatório dos Links"
         <> "\n\n"
         <> L.intercalate "," linkHeader
         <> "\n"
         <> (L.intercalate "\n" $ lsmap <$> (links a))
     )
  where
    residual = jacobianEqNodeHeadGrid env p a f h
    reportEnviroment env =
      L.intercalate "\n" $
        L.intercalate ","
          <$> [ ["Condições Ambiente"],
                ["Fluído", show $ fluidName f],
                ["Viscosidade Dinâmica", show $ fluidViscosity (pressaoBarometrica env / 1000) (temperaturaAmbiente env) f],
                ["Densidade Fluído", show $ fluidDensity (pressaoBarometrica env / 1000) (temperaturaAmbiente env) env f, "kg/m³"],
                ["Gravidade", sf3 (localGravity env), "m/s²"],
                ["Densidade Ar", sf3 (fluidDensity (pressaoBarometrica env / 1000) (temperaturaAmbiente env) env air), "kg/m³"],
                ["Pressão Barométrica", sf3 (pressaoBarometrica env / 1000), "kpa"],
                ["Temperatura", sf3 (temperaturaAmbiente env), "ºC"],
                ["Ponto de Orvalho", sf3 (pontoOrvalho env), "ºC"],
                ["Altura", sf3 (altitude env), "m"]
              ]
      where
        f = fluido env
    reportGrelha =
      let t = L.filter (\e -> (isGrelha . snd $ e) || (isEntry . snd $ e)) (nodes a)
          title = ["Grelhas"]
          header = ["Id", "Vazao", "Velocidade", "Velocidade Min"]
          headerUnit = ["", "L/min", "m³/s", "m/s"]
          proj (i, Tee (TeeConfig _ [s] _) _) = [show i, sf2 vazao, sf2 (vazao / areaS s / 1000 / 60), sf2 0]
            where
              vazao = lks ^. _y
              lks = var i hm
          proj (i, Grelha h w a m) = [show i, sf2 vazao, sf2 (vazao / a / 1000 / 60), sf2 m]
            where
              vazao = lks ^. _y
              lks = var i hm
       in [L.intercalate "\n" $ L.intercalate "," <$> (title : header : headerUnit : (proj <$> t))]
    reportResev =
      let t = L.filter (isReservoir . snd) (nodes a)
          proj (i, Reservatorio tempo) = "Reservatório\n" <> "Volume," <> sf2 (vazao * tempo) <> ",L\n" <> "Tempo de Duração," <> sf2 tempo <> ",min"
            where
              vazao = lks ^. _y
              lks = var i hm
       in (proj <$> t)
    nlmap = M.fromList (findNodesLinks a $ (nodes a))
    res he residual =
      L.intercalate "\n" $
        L.intercalate ";"
          <$> [ [he],
                ["Máximo", show (L.maximum $ abs <$> residual)],
                ["Mínimo", show (L.minimum $ abs <$> residual)],
                ["Norma", show (sqrt $ sum $ fmap (^ 2) residual)]
              ]
    resc = res "Resíduo Node Head" (L.take (M.size f) residual)
    resnh = res "Resíduo Continuity" (L.drop (M.size (f)) residual)
    residuos = "Dados Solução\n\n" <> resnh <> "\n" <> resc
    display (i, l) = i <> "\n" <> L.intercalate "\n" (L.intercalate "," <$> l)
    bomba :: Maybe (String, [[String]])
    bomba = fmap result $ join $ (\(i, (_, _, l)) -> (i,) <$> L.find isBomba l) <$> L.find (\(_, (_, _, l)) -> L.any isBomba l) (links a)
      where
        result (ix, bomba@(Bomba (pn, vn) _)) = ("Bomba", [["Pressão Nominal", sf2 pn, "kpa"], ["Vazão Nominal", sf2 vn, "L/min", sf2 $ vn * 60 / 1000, "m³/h"], ["Potência", maybe "-" (sf2 . potenciaB) dbomba, "cv"], ["Rotação", maybe "-" (sf2 . rotacaoB) dbomba, "rpm"], ["Vazão Operação", sf2 vazao, "L/min", sf2 $vazao * 60 / 1000, "m³/h"], ["Pressão Operação", sf2 pressao, "kpa"], ["Potência Necessária", sf2 $ power, "kW", sf2 $ power * kwtocv, "cv"], ["Eficiência Est.", sf2 eficiencia, "W/W"], ["Potência Est.", sf2 $ power / eficiencia, "kW", sf2 $ power / eficiencia * kwtocv, "cv"]])
          where
            power = pressao * vazao / 60000
            eficiencia = 0.55
            kwtocv = 1.36
            dbomba = M.lookup ((pn / 10, fromIntegral $ round $ vn * 3 / 50)) dadosBomba
            pressao = abs $ pipeElement env undefined vazao bomba
            vazao = var ix fm
    sprinklers = fmap (\(i, e) -> (i, var i hm, e)) $ L.filter (isSprinkler . snd) (nodes a)
    addTee k = maybeToList (M.lookup k nodeLosses)
    sflow = signedFlow a (runIdentity <$> f)
    projectHeader = L.intercalate "\n" ["Projeto," <> projectName header, "Proprietário," <> projectOwner header, "Endereço," <> endereco header, formation ainfo <> "," <> authorName ainfo, "Crea," <> creaNumber ainfo, "Empresa," <> empresa ainfo, "Região Cálculo,"] -- <> regionName rinfo ]
      where
        ainfo = authorInfo header
    headerCalculo =
      projectHeader <> "\n\n" <> "Dados do projeto" <> "\n\n"
        <> L.intercalate
          "\n\n"
          ([reportEnviroment env] <> maybeToList (display <$> bomba) <> reportGrelha <> reportResev {-<> [ unlines $ L.intercalate "," <$> regionReport rinfo ]-})
        <> "\n\n"
        <> residuos
    regionReport r = [["Nome da Região", regionName r], ["Classe de Risco", riskName $ regionRisk r], ["Área da Região", sf2 area', "m²"], ["Densidade de Água", sf2 density, "L/min/m²"], ["Vazão Mínima Área", sf2 minimalFlow, "L/min"], ["Quantidade de Bicos", show nspk, "un"], ["Id", "Pressão", "Vazão", "Vazão Min", "Área"], ["", "kpa", "L/min", "L/min", "m²"]] <> spkReport
      where
        riskName (Standard i) = show i
        riskName (MiscelaneousStorage i _) = show i
        nspk = L.length sprinklers
        area' = abs $ area (convPoly $ fst $ regionBoundary r)
        density = regionRiskDensity r
        minimalFlow = density * area'
        spkReport = (\(ix, p, e@(Sprinkler ((d, k)) (dl) f a)) -> [show ix, sf2 (p ^. _x), sf2 (p ^. _y), sf2 $ coverageArea f * density, sf2 $ coverageArea f]) <$> sprinklers
    nodeLosses = M.fromList . concat . fmap (\(n, Tee t conf) -> (\(ti, v) -> ((n, ti), v)) <$> fittingsCoefficient env hm sflow n t) . filter (isTee . snd) $ nodes a
    nmap = (\ni n@(_, e) -> L.intercalate "," $ ["N-" <> show ni, formatFloatN 4 $ p ni, formatFloatN 4 $h ni, "", ""] ++ expandNode (p ni) e)
      where
        p ni = var ni hm ^. _x
        h ni = var ni pm ^. _1 . _z
    lmap = (\ni n@(h, t, e) -> L.intercalate "," ["T-" <> show h <> "-" <> show t, "", "", formatFloatN 2 $ (abs $ p ni), formatFloatN 2 $ abs (pr h - pr t), "Trecho"])
      where
        pr ni = var ni hm ^. _x
        p ni = var ni fm
    lsmap n@(ni, (h, t, e)) = L.intercalate "\n" $ fmap (L.intercalate ",") $ (mainlink :) $ zipWith3 (expandLink ps (lname <> "-") (p ni)) [0 ..] dgrav els
      where
        p ni = var ni fm
        ps = (var h hm, var t hm)
        mainlink = [lname, "", sf2 $ gravp, sf3 $ negate $ (var h hm ^. _x) - (var t hm ^. _x) + gravDelta (var h pm) (var t pm), "", ""]
          where
            gravp = gravDelta (var h pm) (var t pm)
        lname = "T-" <> show h <> "-" <> show t
        grav = (if isJust (M.lookup (h, ni) nodeLosses) then (var h pm :) else id) (var ni lpos <> [var t pm, var t pm])
        dgrav = zipWith gravDelta grav (drop 1 grav)
        gravDelta i j = gravityEffect ps env (i ^. _1 . _z - j ^. _1 . _z)
        els = e -- addTee (h,ni) <> e <> addTee (t ,ni)
    fm = runIdentity <$> f
    hm = h
    pm = M.fromList (nodesPosition p)
    lpos = M.fromList (linksPosition p)
    nodeHeader = ["ID", "Pressão (kpa)", "Altura (m)", "Vazão (L/min)", "Perda (kpa)", "Elemento"]
    expandNode p (Sprinkler ((d, k)) (dl) f a) = ["Sprinkler"]
    expandNode _ (Reservatorio _) = ["Reservatorio"]
    expandNode _ (Grelha _ _ _ _) = ["Grelha"]
    expandNode _ (Tee (TeeConfig i r c) _) = [show c]
    expandNode _ (Open 0) = ["Tampa"]
    expandNode _ (Open i) = ["Hidrante"]
    expandNode _ i = []
    linkHeader = ["SubTrecho", "Diametro (m)", "Gravidade(kpa)", "Atrito(kpa)", "Elemento", "Comprimento (m)"]
    expandLink ps st f i dg t@(Tubo s dl _) = [st <> show i, secao s, sf2 dg, sf2 $ ktubo env ps joelhos t (abs f), "Tubo ", sf3 dl]
      where
        secao (Rectangular h w) = sf3 h <> "x" <> sf3 w
        secao (Circular d) = sf3 d
    expandLink _ st f i dg t@(Turn _) = [st <> show i, "", sf2 dg, "", "Turn", ""]
    expandLink ps st f i dg b@(Bomba (d, _) dl) = [st <> show i, "", sf2 dg, sf2 $ pipeElement env ps f b, "Bomba"]
    expandLink ps st f i dg j@(Joelho _ (TabelaPerda (d) (_, _, c) _)) = [st <> show i, sf3 d, sf2 dg, sf2 $ktubo env ps joelhos j (abs f), "Joelho " <> c]
    expandLink ps st f i dg j@(Perda (TabelaPerda (d) (s, m, c) _)) = [st <> show i, sf3 d, sf2 dg, sf2 $ktubo env ps joelhos j (abs f), s <> m <> c]


solveSystem :: (Show d,RealFloat d,Coord b t, Show1 b, Show1 (Ang t),Show1 t , PreSys b, Additive t, MultiLinear (Ang t), Traversable (LinkDomain b) , Traversable (NodeDomain b), Additive (Enviroment b))
            => (forall a . (Show a,RealFloat a) => Grid b a ) 
            -> (forall a . (Show a,RealFloat a) => CoordinateGrid t a -> Grid b a -> M.Map Int (LinkDomain b a) -> M.Map Int (NodeDomain b a) -> [a] )  
            -> IO  (Iteration t b d) 
solveSystem testResistor thermalEq = do 
  putStrLn "System:"
  putStrLn (renderEquations $ displayEquation  testResistor (thermalEq (fst $ upgradeGrid 0 1 testResistor)) )
  let iter = solveIter (initIter  testResistor (fst $ upgradeGrid 0 1 testResistor) zero) thermalEq
  putStrLn "Solution:"
  putStrLn $ renderResults iter
  putStrLn "Residuals:"
  let posres = printResidual iter thermalEq 
  putStrLn (unlines ((\(ix,v) -> show ix <> ". " <> showFFloat (Just 4) v "") <$> zip [0..] posres))
  return iter


