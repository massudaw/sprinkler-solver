{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Project where

import Grid
import Eletric
import Rotation.SO3 hiding (rotM)
import System.Process
import Control.Monad
import qualified Data.Text.IO as T
import Position
import Data.Maybe
import Sprinkler
import Tee
import Element
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Ord
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Mecha

import Control.Lens

import Diagrams.Prelude hiding (end)


path i h t  l = (i,h,t,  l)

jd dir d = Joelho (Just d) ("Conexao","Joelho","90")  dir 100

initIterEletric ::  Num a => Grid Eletric a -> Iteration Eletric a
initIterEletric g = Iteration ( zip (fmap (\(i,_,_,_)-> i) (links g)) (replicate 10 20 <>  repeat 4 )) ( zip ( fmap fst $ filter (not .isGround .snd)  (nodesFlow g))(repeat 100))  g

initIterElement ::  Num a =>Grid Element a -> Iteration Element a
initIterElement g = Iteration ( zip (fmap (\(i,_,_,_)-> i) (links g)) (replicate 10 20 <>  repeat 4 )) ( zip ( fmap fst $ filter (not .isReservoir.snd)  (nodesFlow g))(repeat 100))  g

makeIter i j g = initIterElement (realToFrac  <$>  upgradeGrid i j g)

data ProjectHeader
  = ProjectHeader { projectName :: String
    , endereco :: String
    , projectOwner :: String
    , authorInfo :: Author
    , regionInfo :: Region
    }

data Region
  = Region  { regionName :: String
    , regionFile :: String
    , regionView :: [(String,String)]
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



solveCircuit :: Grid Eletric Double  -> Iteration Eletric Double
solveCircuit g = solveIter (initIterEletric (fmap realToFrac g)) circuitEq

-- testResistor :: SR.Expr

displayModel (header,model) = do
  let iter = makeIter 0 1 model
      fname  = regionFile $ regionInfo header
  T.writeFile (fname <> "-temp.scad") $openSCAD (drawIter iter )
  callCommand $ "mv " <> (fname <> "-temp.scad") <>  "  " <> (fname <> ".scad")


solveModel (header ,model) = do
  let
       iter = solveIter (makeIter 0 1 model ) jacobianEqNodeHeadGrid
       fname  = regionFile $ regionInfo header
  reportIter header 0 iter
  print $ "renderReport" <> (fname <> ".csv")
  let sofficec = "soffice --convert-to xls --infilter=csv:\"Text - txt - csv (StarCalc)\":59/44,34,0,1,,1033,true,true  " <> fname <> ".csv"
  putStrLn sofficec
  -- callCommand $ sofficec
  print $ "renderReport " <> (fname <> ".xls")
  let scadFile = openSCAD (drawIter iter )
  T.writeFile (fname <> "-temp.scad") scadFile
  let movefile = "mv " <> (fname <> "-temp.scad") <>  "  " <> (fname <> ".scad")
  callCommand movefile
  print $ "renderSCAD" <> (fname <> ".scad")
  mapM (\v -> do
      let command  = "openscad -o " <> fname <> "-" <> fst v<> ".png " <> fname <> ".scad  " <> snd v
      putStrLn command
      callCommand  command) (regionView $ regionInfo header)



totalHead a p va =   p/(g*rho) + v^2/(2*g)
  where g = 9.81
        rho = 1000
        v = va/a

data CyclePoint a b
  = CloseLink a
  | BranchLink b
  deriving(Eq,Show,Ord)


nodesFlowSet g = findNodesLinks g $ fmap (\n@(ni,_) -> (ni,n)) $nodesFlow $ g


upgradeGrid :: Int -> Int -> Grid Element Double -> Grid  Element Double
upgradeGrid ni li a = a {shead = M.toList nodesPos, linksPosition = M.toList linksPos}
  where
    (nodesPos,linksPos) =  snd $ runState (do
                      modify (<> (M.singleton ni (0,SO3 $ rotM 0), mempty))
                      locateGrid lmap nmap ni (0,SO3 $rotM 0 ) (Left $ var li lmap ))
                        (mempty,mempty)
    lmap = M.fromList (fmap (\l@(li,_,_,_)-> (li,l))  $ links a)
    nmap = M.fromList (findNodesLinks a $ fmap (\n@(ni,_) -> (ni,n)) $ (nodesFlow a) )

recurse render r@(Right l@(ni,h,t,e)) = do
  lift $ modify (<> (S.empty,S.singleton ni))
  i <- fst <$> lift  get
  linkmap <- fst <$> ask
  let nexts = S.toList $ S.difference (S.fromList [h,t]) i
  ti <- mapM (recurse render . Left . flip var linkmap) nexts
  return $ render r  : concat ti
recurse render r@(Left n@(ni,lks,e)) = do
  lift $ modify (<>(S.singleton ni,S.empty))
  s <- snd <$> lift  get
  nodemap <- snd <$> ask
  let nexts = S.toList $ S.difference lks  s
  ti <- mapM (recurse render . Right . flip var nodemap) nexts
  return $ render r : concat ti

sf2 = show -- formatFloatN 2
sf3 = show -- formatFloatN 3

reportIter :: ProjectHeader -> Int -> Iteration Element Double -> IO ()
reportIter header i iter@(Iteration f h a)  = do
    let name = regionFile $ regionInfo header
    writeFile (name <> ".csv")  $(headerCalculo <> "\n\n" <> "Relatório dos Nós" <> "\n\n" <>( L.intercalate "\n"    $ (L.intercalate ","  nodeHeader :) (evalState (runReaderT (do
           nodemap  <- fst <$> ask
           recurse (either nmap lmap) (Left $ fromJustE "no node" $ M.lookup i nodemap )) (M.fromList $ fmap (\(i,(j,k))-> (i,(i,j,k))) $ findNodesLinks a (nodesFlow a) ,M.fromList $ fmap (\l@(i,h,t,e)-> (i,l))$ links a )) (S.empty,S.empty))) <>
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
    vazao = fromJustE "no flow for node 1 " $ M.lookup 1 (M.fromList f)
    bomba :: Element Double
    bomba@(Bomba (pn,vn)   _) = fromJustE "no pump"$ join $ L.find isBomba . (\(_,_,_,l) -> l)<$> L.find (\(_,_,_,l) -> L.any isBomba l ) (links a )
    pressao = abs $ pipeElement vazao bomba
    sprinklers =  fmap (\(i,e) -> (i,fromJustE "no sprinkler" $ M.lookup i  (M.fromList h ),e)) $  L.filter (isSprinkler . snd) (nodesFlow a)
    areaSprinkler =  sum $ fmap coverageArea ((\(_,_,i) -> areaCobertura i)<$> sprinklers)
    (_,_,Sprinkler (Just (dsp,ksp))  _ _ _) = head sprinklers
    spkReport = L.intercalate "\n" $ L.intercalate ","  . (\(ix,p,e@(Sprinkler (Just (d,k )) (dl) f a ))-> [show ix , formatFloatN 2 p ,   formatFloatN 2 $ k*sqrt p]  ) <$>    sprinklers
    nodeLosses = M.fromList . concat .fmap (\(n,Tee t conf ) -> (\(ti,v)-> ((n,ti),v)) <$> classifyTeeEl conf (fmap (\x -> x/1000/60) $ var n  sflow) t) .  filter (isTee .snd) $ nodesFlow a
    addTee k = maybeToList (M.lookup k nodeLosses)
    sflow = signedFlow a  (M.fromList f)

    projectHeader =  L.intercalate "\n" ["Projeto," <> projectName header   ,"Proprietário," <> projectOwner header , "Endereço," <> endereco header, formation ainfo <> "," <> authorName ainfo , "Crea," <> creaNumber ainfo , "Empresa," <>   empresa ainfo , "Região Cálculo," <> regionName (regionInfo header)]
        where ainfo = authorInfo header
    headerCalculo = projectHeader <> "\n\n" <> "Dados do projeto" <> "\n\n" <> L.intercalate "\n\n"
            ["Bomba\n" <> "Pressão Nominal," <> sf2 pn <> ",kpa\n" <> "Vazão Nominal," <> sf2 vn <> ",L/min\n" <> "Potência,185,cv\n" <> "Vazão Operação," <> sf2 vazao <> ",L/min\n" <>  "Pressão Operação," <> sf2 pressao <> ",kpa,"
            ,"Reservatório\n" <> "Volume," <> sf2 (vazao * tempo) <> ",L\n" <> "Tempo de Duração," <> sf2 tempo<> ",min"
            ,"Sprinkler\nTipo,ESFR\n"  {-<> "Diâmetro,25,mm\n" <> "Area,9,m²\n" <> -}<> "K," <> sf2 ksp <> ",L/min/(kpa^(-1/2))\n"<> "Vazão Mínima," <> sf2 (ksp*sqrt 350) <>  ",L/min\n" <> "Pressão Mínima,350,kpa\n" <> "Area Projeto;" <> sf3 areaSprinkler <>  ";m²\n" <> "Quantidade Bicos," <> show (L.length sprinklers) <> ",un" ] <>  "\nId,Pressão(kpa),Vazão(L/min)\n" <> spkReport <>  "\n\n" <>residuos

    nmap = (\n@(ni,s,e) -> L.intercalate "," $ ["N-"<> show ni,formatFloatN 2 $ maybe 0 id $p ni , formatFloatN 2 $h  ni,"",""] ++  expandNode (p ni) e )
        where p ni =  varM ni hm
              h ni =  fst ( (fromJustE $"no position pressure for node " <> show ni) (varM ni  pm)) ^. _z
    lmap = (\n@(ni,h,t,e) ->  L.intercalate "," ["T-"<> show h <>  "-" <> show t ,"","",formatFloatN 2 $  maybe 0 id (abs <$> p ni) ,formatFloatN 2  $ abs (pr h - pr t),"Trecho"])
        where pr ni =  maybe 0 id (varM ni hm)
              p ni =  varM ni fm
    lsmap n@(ni,h,t,e) = L.intercalate "\n" $ fmap (L.intercalate ",") $ (mainlink:) $ expandLink (lname <>  "-") (p ni) <$>  zip [0..] (addTee (h,ni) <> e <> addTee (t ,ni))
        where p ni =  varM ni fm
              mainlink = [lname  , "", show $ fromMaybe 0 (varM h hm ) - fromMaybe 0 (varM t hm),"", ""]
              lname = "T-"<> show h <>  "-" <> show t
    fm = M.fromList f
    hm = M.fromList h
    pm = M.fromList (shead a )
    nodeHeader = ["ID","Pressão Dinâmica (kpa)","Altura (m)","Vazão (L/min)","Perda (kpa)","Elemento"]
    expandNode (Just p) (Sprinkler (Just (d,k)) (dl) f a ) = ["Sprinkler"]
    expandNode _ (Reservatorio  _ )  = ["Reservatorio"]
    expandNode _ (Tee (TeeConfig i r db dr c) _ )  = ["Tê"]
    expandNode _ (Open 0 )  = ["Tampa"]
    expandNode _ (Open i )  = ["Hidrante"]
    expandNode _ i = []
    linkHeader = ["SubTrecho","Diametro (m)","Perda (kpa)","Elemento","Comprimento (m)"]
    expandLink st (Just f) (i,t@(Tubo (Just d ) dl  _)) = [st <> show i ,  sf3 d , sf2$   Grid.ktubo t*(abs f)**1.85,"Tubo " , sf3 dl ]
    expandLink st (Just f) (i,t@(Turn   _)) = [st <> show i ,  "" , "","Turn" , "" ]
    expandLink st (Just f) (i,b@(Bomba (d,_)  dl  )) = [st <> show i, "", sf2 $ pipeElement f b,"Bomba"]
    expandLink st (Just f) (i,j@(Joelho (Just d)  (_,_,c)  _ _ ) ) = [st <> show i , sf3 d, sf2 $Grid.ktubo j*(abs f)**1.85,"Joelho " <> c]
    expandLink st (Just f) (i,j@(Perda (Just d)  (s,m,c)   _ ) ) = [st <> show i , sf3 d, sf2 $Tee.ktubo j (abs f/1000/60),s <> m <> c]




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


findNodesLinks grid = fmap (\(i,n) -> (i,(var i nodeMapSet,n)))
    where nodeMapSet = fmap S.fromList $ M.fromListWith mappend $ concat $ (\(l,h,t,_) -> [(h,[l ]),(t,[l ])]) <$> links grid
