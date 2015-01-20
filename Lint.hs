module Lint where
import Grid
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.Monoid
import Element
import Sprinkler
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Set as S

--lintLinks :: (Show a,Ord a) => Grid a -> [String]
lintLinks grid =     mapM_ checkLinks (links grid)
  where
        checkLinks c@(n,h,t,elems) = do
             F.foldl' (\i j -> do
                   idia <- i
                   case j of
                      (DiameterChange dii dio _ ) ->  when (diametroE idia /= dii ) $
                          tell $ ["diametro elem " ++ show idia ++ " is different from "++ show j ]
                      i ->  when (diametroE idia /= diametroE j) $
                          tell $ ["diametro elem " ++ show idia ++ " is different from "++ show j ]
                   return j) (return $ head elems)  (tail elems)

nodeConnective grid = do
  let   nds =  (fst <$> nodesFlow grid )<>  (fst <$> shead grid)
        hasNodes = filter (not .(`S.member` S.fromList nds).fst) (M.toList $ nodeUsage)
        nodeMap = M.fromList (zip  nds (repeat []))
        nodeUsage = foldr (\(l,h,t,_)-> M.insertWith mappend h [l] .M.insertWith mappend  t [l] ) M.empty (links grid)
        orphanNodes = filter ((<1) . length  . snd ) (M.toList nodeUsage)
  mapM_ (\i -> tell $ ["No node for links" ++ show i ]) hasNodes
  mapM_ (\i -> tell $ ["OrphanNode " <> show i]) orphanNodes


lintInitialConditions iter = snd $ runIdentity $ runWriterT $ do
  lintInitialTee iter


lintInitialTee  iter  = do
  let fl = ((/(1000*60)) <$> M.fromList (flows iter))
  mapM_ (\(n,Tee config)-> lintInitTee (var n $ signedFlow (grid iter) fl) config) ( filter (isTee .snd ) $  (nodesFlow (grid iter)))

lintInitTee flowMap  t =  do
  let flow = fmap (\i -> maybe (error ("no variable " ++ show i ++ " in map " ++ show flowMap ))  id  $ M.lookup  i flowMap) (teeConfig t)
  classifyFlow flow
  where
        [rli,bi,rri] = teeConfig t
        classifyFlow bl@[rls,bs,rrs]
          |  rls > 0 && bs <= 0 && rrs <= 0 = return ()
          |  rrs > 0 && bs <= 0 && rls <= 0 =  return ()
          |  rls >= 0 && bs >= 0 && rrs < 0 = return ()
          |  rrs >= 0 && bs >= 0 && rls < 0 = return ()
          |  bs > 0 && rrs <= 0 && rls <= 0 = return ()
          |  bs < 0 && rrs >= 0 && rls >= 0 = return ()
          | bs < 0 && rrs < 0 && rls <0 = tell ["no case for all branch list negative " ++ show  t ++ show bl ]
          | bs > 0 && rrs > 0 && rls >0 = tell ["no case for all branch list positive " ++ show  t ++ show bl ]
          | bs == 0 && rrs == 0 && rls == 0 = tell ["no case for all branch list zero " ++ show  t ++ show bl]
          | otherwise =  tell ["no case for this branch list " ++ show  t ++ show bl ]


lintGridElements grid = snd $ runIdentity $ runWriterT $ do
    lintLinks grid
    lintTee grid
    nodeConnective grid


-- lintTee :: (Show a,Ord a) => Grid a -> [String]
lintTee grid =   mapM_ checkLinks tees
  where tees = filter (isTee .snd) (nodesFlow grid)
        checkLinks c@(n,Tee (TeeConfig tc@[rl,b,rr] r db dr m)) = do
            let lks@[lrl,lb,lrr] = fmap (\i -> head . (\(h,t,e) -> if n == h then e else reverse e ) $ var  i linkMap)  tc
            when (diametroE lrr /= Just dr ) $
              tell $ ["diametro right run " ++ show rr ++ " is different from tee "++ show n ++ " " ++ show db ++ " /= " ++ show (diametroE lrr)]
            when (diametroE lrl /= Just dr )  $
              tell $ ["diametro left run " ++ show rl ++ " is different from tee " ++ show n ++ " " ++ show db ++ " /= " ++ show (diametroE lrl)]
            when (diametroE lb /= Just db) $
              tell $ ["diametro branch " ++ show b ++ " is different from tee "++ show n ++ " " ++ show db ++ " /= " ++ show (diametroE lb)]
            when ( diametroE lrr /= diametroE lrl )$
              tell $ ["diametro runs " ++ show rl ++ "," ++ show rr ++ " are different from tee "++ show n ++ " " ++ show (diametroE lrl ) ++ " /= " ++ show (diametroE lrr)]
        linkMap = M.fromList $ fmap (\(li,h,t,e) ->   (li,(h,t,e))) (links grid)


