{-# LANGUAGE RecursiveDo,OverloadedStrings ,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Project where

import Grid
import qualified Data.Set as S
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

import Diagrams.Prelude hiding (end)


path i h t  l = (i,h,t,  l)

jd dir d = Joelho (Just d) ("Conexao","Joelho","90")  dir 100

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
  = Region  { regionName :: String
    , regionFile :: String
    , regionView :: [(String,String)]
    , regionBoundary :: [V2  Double]
    }
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
cruz i j k = tee TeRunL [tubod 0.05 0.01,tee TeRunR  k j ] i
tubod  d l  = Tubo (Just d) l 100

joelhoR  = Joelho Nothing ("Conexao","Joelho","90") right90  100
joelho  = joelhoR
joelhoL  = Joelho Nothing ("Conexao","Joelho","90") left90  100
joelho45  = joelho45R
joelho45R  = Joelho Nothing ("Conexao","Joelho","45") right45  100
joelho45L  = Joelho Nothing ("Conexao","Joelho","45") left45  100


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
      reach = S.fromList $  concat $ fmap (\(i,j) -> [i,j]) $ concat $ concat $ fmap (\v -> connected 0 v   graph  ) (take 1 t)
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

connected x y g = helper x y g (S.singleton x)
  where
    helper a b g visited
        | a == b    = [[]]
        | otherwise = [(a,c):path | c <- next ,  path <- helper c b g nextS]
      where
        next = filter (not . (`S.member` visited))  (g A.! a)
        nextS = foldr S.insert visited  next

renderModel (header,model) = do
  let fname  = baseFile header
      dxfname = fname <> ".DXF"
  Right f <- readDXF dxfname
  let calc_bounds  =     filter ((== "calc_area").layer. eref) $ entities f
      projBounds (Entity n o (LWPOLYLINE _ _ _ polygon )) = fmap fst polygon
      regions (i,b) =  Region (show i) (baseFile header  <> "-" <> show i ) [] b
      modelg = fst $ upgradeGrid 0 1 $ model
  mapM (\r@(Region _ _ _ p) -> do
     solveModel (header , initIter $  fst $ upgradeGrid 0 1 $ filterBounded p  $ modelg ,r)) (regions <$> zip [0..] (projBounds <$> calc_bounds))
  renderDXF  (baseFile header) fname  (drawGrid  modelg)
  drawSCAD (baseFile header) (baseFile header) modelg


solveModel (header ,model,region ) = do
  let fname  = regionFile region
  let
       iter = solveIter (fmap realToFrac model ) jacobianEqNodeHeadGrid
  reportIter header region 0 iter
  print $ "renderReport: " <> (fname <> ".csv")
  -- let sofficec = "soffice --convert-to xls --infilter=csv:\"Text - txt - csv (StarCalc)\":59/44,34,0,1,,1033,true,true  " <> fname <> ".csv"
  -- putStrLn sofficec
  -- callCommand $ sofficec
  -- print $ "renderReport: " <> (fname <> ".xls")
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


totalHead a p va =   p/(g*rho) + v^2/(2*g)
  where g = 9.81
        rho = 1000
        v = va/a

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
  let nexts = S.toList $ S.difference lks  s
  ti <- mapM (\i -> recurse render i . Right . flip var nodemap $ i ) nexts
  return $ render ni r : concat ti

sf2 = show -- formatFloatN 2
sf3 = show -- formatFloatN 3


reportIter :: ProjectHeader -> Region -> Int -> Iteration Element Double -> IO ()
reportIter header rinfo i iter@(Iteration f h a)  = do
    let name = regionFile $ rinfo
    writeFile (name <> ".csv")  $(headerCalculo <> "\n\n" <> "Relatório dos Nós" <> "\n\n" <>( L.intercalate "\n"    $ (L.intercalate ","  nodeHeader :) (evalState (runReaderT (do
           nodemap  <- fst <$> ask
           recurse (\ni -> either (nmap ni) (lmap ni)) i (Left $ fromJustE "no node" $ M.lookup i nodemap )) (M.fromList $  findNodesLinks a (nodesFlow a) ,M.fromList $ links a )) (S.empty,S.empty))) <>
            "\n\n" <> "Relatório dos Links" <> "\n\n" <> L.intercalate "," linkHeader <>"\n" <> (L.intercalate "\n" $ lsmap <$> (links a)))
  where
    residual = printResidual iter jacobianEqNodeHeadGrid
    Reservatorio tempo = snd $ justError "Reservatório não encontrado" $ L.find (isReservoir .snd) (nodesFlow a)
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
      where result (ix,bomba@(Bomba (pn,vn) _ )) = ("Bomba", [["Pressão Nominal",  sf2 pn , "kpa"] ,["Vazão Nominal", sf2 vn , "L/min"] ,["Potência", sf2 185 ,"cv"], ["Vazão Operação",sf2 vazao , "L/min"] ,["Pressão Operação", sf2 pressao , "kpa"]])
              where
                pressao = abs $ pipeElement vazao bomba
                vazao = fromJustE "no flow for node 1 " $ join $ M.lookup ix fm

    sprinklers =  fmap (\(i,e) -> (i,fromJustE "no sprinkler" $ join $ M.lookup i  hm ,e)) $  L.filter (isSprinkler . snd) (nodesFlow a)
    spkReport = L.intercalate "\n" $ L.intercalate ","  . (\(ix,p,e@(Sprinkler (Just (d,k )) (dl) f a ))-> [show ix , formatFloatN 2 p ,   formatFloatN 2 $ k*sqrt p]  ) <$>    sprinklers
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t conf ) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTeeEl conf (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow a
    addTee k = maybeToList (M.lookup k nodeLosses)
    sflow = signedFlow a  (justError "no flow " .runIdentity .getCompose <$> M.fromList f)

    projectHeader =  L.intercalate "\n" ["Projeto," <> projectName header   ,"Proprietário," <> projectOwner header , "Endereço," <> endereco header, formation ainfo <> "," <> authorName ainfo , "Crea," <> creaNumber ainfo , "Empresa," <>   empresa ainfo , "Região Cálculo," <> regionName rinfo ]
        where ainfo = authorInfo header
    headerCalculo = projectHeader <> "\n\n" <> "Dados do projeto" <> "\n\n" <> L.intercalate "\n\n"
            (maybeToList (display <$>bomba )<> [
            "Reservatório\n" <> "Volume," <> sf2 (vazao * tempo) <> ",L\n" <> "Tempo de Duração," <> sf2 tempo<> ",min"
            {-,"Sprinkler\nTipo,ESFR\n"  {-<> "Diâmetro,25,mm\n" <> "Area,9,m²\n" <> -}<> "K," <> sf2 ksp <> ",L/min/(kpa^(-1/2))\n"<> "Vazão Mínima," <> sf2 (ksp*sqrt 350) <>  ",L/min\n" <> "Pressão Mínima,350,kpa\n" <> "Area Projeto;" <> sf3 areaSprinkler <>  ";m²\n" <> "Quantidade Bicos," <> show (L.length sprinklers) <> ",un" -} ]) <>  "\nId,Pressão(kpa),Vazão(L/min)\n" <> spkReport <>  "\n\n" <>residuos

    nmap = (\ni n@(s,e) -> L.intercalate "," $ ["N-"<> show ni,formatFloatN 4 $ maybe 0 id $p ni , formatFloatN 4 $h  ni,"",""] ++  expandNode (p ni) e )
        where p ni =  join $ varM ni hm
              h ni =  fst ( (fromJustE $"no position pressure for node " <> show ni) (varM ni  pm)) ^. _z
    lmap = (\ni n@(h,t,e) ->  L.intercalate "," ["T-"<> show h <>  "-" <> show t ,"","",formatFloatN 2 $  maybe 0 id (abs <$> p ni) ,formatFloatN 2  $ abs (pr h - pr t),"Trecho"])
        where pr ni =  maybe 0 id (join $ varM ni hm)
              p ni =  join $ varM ni fm
    lsmap n@(ni,(h,t,e)) = L.intercalate "\n" $ fmap (L.intercalate ",") $ (mainlink:) $ zipWith3 (expandLink (lname <>  "-") (p ni) ) [0..] dgrav els
        where p ni =  join $ varM ni fm
              mainlink = [lname  , "", sf2 $ gravp  ,show $ negate $ fromMaybe 0 (join $ varM h hm ) - fromMaybe 0 (join $ varM t hm) + gravDelta (var h pm ) (var t pm) ,"", ""]
                where gravp =gravDelta (var h pm ) (var t pm)
              lname = "T-"<> show h <>  "-" <> show t
              grav =  (if isJust (M.lookup (h,ni) nodeLosses) then(var h pm :)else id ) (var ni lpos  <> [var t pm,var t pm])
              dgrav = zipWith gravDelta grav (drop 1 grav)
              gravDelta i j = (i ^. _1._z  - j ^. _1._z)* 9.81
              els = addTee (h,ni) <> e <> addTee (t ,ni)
    fm = runIdentity . getCompose <$> M.fromList f
    hm = runIdentity . getCompose <$> M.fromList h
    pm = M.fromList (shead a )
    lpos = M.fromList (linksPosition a )
    nodeHeader = ["ID","Pressão Dinâmica (kpa)","Altura (m)","Vazão (L/min)","Perda (kpa)","Elemento"]
    expandNode (Just p) (Sprinkler (Just (d,k)) (dl) f a ) = ["Sprinkler"]
    expandNode _ (Reservatorio  _ )  = ["Reservatorio"]
    expandNode _ (Tee (TeeConfig i r _ db dr c) _ )  = ["Tê"]
    expandNode _ (Open 0 )  = ["Tampa"]
    expandNode _ (Open i )  = ["Hidrante"]
    expandNode _ i = []
    linkHeader = ["SubTrecho","Diametro (m)","Gravidade(kpa)", "Atrito(kpa)","Elemento","Comprimento (m)"]
    expandLink st (Just f) i dg t@(Tubo (Just d ) dl  _) = [st <> show i ,  sf3 d , sf2 dg ,sf2$   Element.ktubo t*(abs f)**1.85,"Tubo " , sf3 dl ]
    expandLink st (Just f) i dg t@(Turn   _) = [st <> show i ,   "" ,sf2 dg , "","Turn" , "" ]
    expandLink st (Just f) i dg b@(Bomba (d,_)  dl  ) = [st <> show i, "",sf2 dg , sf2 $ pipeElement f b,"Bomba"]
    expandLink st (Just f) i dg j@(Joelho (Just d)  (_,_,c)  _ _ )  = [st <> show i , sf3 d,sf2 dg , sf2 $Element.ktubo j*(abs f)**1.85,"Joelho " <> c]
    expandLink st (Just f) i dg j@(Perda (Just d)  (s,m,c)   _ )  = [st <> show i , sf3 d,sf2 dg , sf2 $Tee.ktubo j (abs f/1000/60),s <> m <> c]





{-
expandGrid a = runState (recurseNode  [] (lookNode (fst $ head sortedHeads))) (S.empty,S.empty)
  where
    recurseNode path t@(n,l@(s,_)) = do
       (visited,visitedNode) <- get
       let -- not visited nodes
           nextNodes = filter (not . (`S.member` visitedNode)) $ fmap (lookLinkNode n) nextLinks
           -- not visited Links
           nextLinks =  fmap (flip var linkMap) $ filter (not . (`S.member` visited )) (fmap fst $ S.toList s)
           -- return links, not visited links but visited nodes
           backLinks =  filter ((`S.member` visitedNode)  . lookLinkNode  n ) $ fmap (flip var linkMap) $ filter (not . (`S.member` visited )) (fmap fst $ S.toList s)
           fnodes = nextNodes
       modify (<> (S.map fst  s,S.fromList nextNodes))
       tnodes <- traverse (\p  -> fmap (\j -> Left (BranchLink (fromJustE "no link for brach " $ L.find (\(l,h,t,_) -> h == fst p  || t == fst p ) nextLinks )) : j) . recurseNode (n:path) $ p ) $ L.sortBy (flip (comparing (\(i,p) ->  totalHead 0.08 (var i nodePressures ) ((/2) $ sum $ fmap (abs .snd ) $ S.toList $fst p)))) $ fmap lookNode fnodes
       return  $ Right (path,totalHead 0.08 (var n nodePressures) ((/2) $ sum $ fmap (abs .snd ) $ S.toList $fst l),t) :  (fmap (Left . CloseLink ) backLinks <>  concat  tnodes)
    lookLinkNode bn (l,h,t,_) = if bn == h then t else h
    nodePressures = M.fromList $ pressures a <> (fmap (\(j,i) -> (j,9.81* ( fst i ^. _z))) $ shead (grid a))
    lookNode i = (i,var i nodeMap)
    linkflow = M.fromList (flows a)
    sortedHeads = L.sortBy (flip (comparing (\(i,p) ->  totalHead 0.08 p ((/2) $ sum $ fmap (abs .snd ) $ S.toList $ fst $ var i nodeMap))))  (pressures a)
    nodeMap =  fmap (\(s,i) -> (S.map (\si-> (si,var si linkflow)) s, i) ) $ M.fromList $findNodesLinks (grid a) $ (fmap (Left ) <$>  (shead $ grid a )) <> (fmap Right <$> (nodesFlow $ grid a))
    linkMap = M.fromList $ (\l@(i,_,_,_) -> (i,l)) <$> (links $ grid a)
-}

findNodesLinks grid = fmap (\(i,n) -> (i,(var i nodeMapSet,n)))
    where nodeMapSet = fmap S.fromList $ M.fromListWith mappend $ concat $ (\(l,(h,t,_)) -> [(h,[l ]),(t,[l ])]) <$> links grid
