{-# LANGUAGE MultiParamTypeClasses,OverloadedStrings,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Backend.Graphviz (renderGraph ,Statements) where

import Eletric
import Data.Functor.Identity
import Thermal
import Numeric
import Domains
import qualified Data.Sequence as Seq

import Data.GraphViz.Attributes (shape)
import Data.GraphViz.Attributes.Complete  (Shape(..),Number(..),Attribute(..) ,Label(..))
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Commands
import qualified Data.Map.Monoidal as MM

import Linear.V3
import qualified Data.Text.Lazy as T
import Data.Monoid

data Statements = Statements (MM.MonoidalMap Int [Attribute]) (MM.MonoidalMap (Int,Int) (MM.MonoidalMap Int [Attribute])) deriving (Show)

node n a = Statements (MM.singleton n a) MM.empty

edge l h t a = Statements MM.empty (MM.singleton (h,t)  (MM.singleton  l a ))

instance Semigroup Statements where
  Statements i j <> Statements k l =  Statements (i <> k ) ( j <> l )

instance Monoid Statements where
  mempty = Statements mempty mempty

renderGraph ::  Statements -> FilePath -> IO FilePath
renderGraph (Statements n l) f =  do
  runGraphvizCommand Fdp (DotGraph { strictGraph = False
         , directedGraph = False , graphID = Just (Num (Int 0))
         , graphStatements = Seq.fromList  $ ((\(n, a) -> DN $ DotNode n a ) <$> MM.toList n ) <> 
                  ( concatMap (\((h,t), a) -> ((DE . DotEdge h t) . snd) <$>  MM.toList a) $ MM.toList l) }) Png f

formatFloatN numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

instance RBackend Statements where
  type TCoord Statements = V3 
  type TField Statements = Double
  errorItem = undefined
  transformElement (r,s)= id
  statements = foldr (<>) mempty

instance Target  Eletric Statements where
  renderNode ni Node  =  node ni [shape MDiamond]
  renderNode ni Ground  = node ni [shape MSquare]
  renderNode ni i = error $ show (ni,i)

  renderLink l h t (VoltageSource v ) = edge  l h t [Label $ StrLabel $ T.pack $ show v <> "V"] 
  renderLink l h t (Resistor r ) = edge l h t [Label $ StrLabel $ T.pack $show r <> "ohm" ] 

instance Target  Thermal Statements where
  renderNode  ni ThermalNode = node ni [Label (StrLabel "Isolated") , shape Egg]
  renderNode  ni (Ambient v) = node ni [Label (StrLabel "Ambient") , shape MDiamond]
  renderNodeSolve  (Identity f) ni sys = node ni [XLabel $ StrLabel $ T.pack $ (formatFloatN 2 f ) <> " ºC"]
  renderLink  l h t (HeatFlow v ) = edge l h t [Label $ StrLabel $ T.pack $ show v <> " W/m2"] 
  renderLink  l h t (Conductor v ) = edge l h t [Label $ StrLabel $ T.pack $ show v <> " R"] 
  renderLinkSolve  (Identity f ) l h t (Conductor v ) = edge l h t [XLabel $ StrLabel $ T.pack $ (formatFloatN 2 f) <> " W/m2"] 
