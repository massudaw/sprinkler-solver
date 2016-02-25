{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Graphviz (renderGraph ) where

import Eletric
import Thermal
import Position
import qualified Data.Sequence as Seq

import Data.GraphViz.Attributes (shape)
import Data.GraphViz.Attributes.Complete  (Shape(..),Number(..),Attribute(..) ,Label(..))
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Commands

import Linear.V3
import Data.Text.Lazy as T
import Data.Monoid

renderGraph ::  [DotStatement Int ] -> FilePath -> IO FilePath
renderGraph i f =  do
  runGraphviz (DotGraph { strictGraph = False
         , directedGraph = False , graphID = Just (Num (Int 1))
         , graphStatements = Seq.fromList ({-(GA $ GraphAttrs $ [Layout Neato]) :-} i)}) Png f

renderElemMecha  _ (_,(_,(ni,Node ))) = [DN $ DotNode ni [shape MDiamond]]
renderElemMecha  _ (_,(_,(ni,Ground ))) = [DN $ DotNode ni [shape MSquare]]
renderElemMecha  _ i = error $ show i

renderLinkMecha (f,nf) (h,t) nis ni (VoltageSource v ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $ show v <> "V"] ]
renderLinkMecha (f,nf) (h,t) nis ni (Resistor r ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $show r <> "ohm" ] ]

instance RBackend [DotStatement Int] where
  type TCoord [DotStatement Int]= V3 Double
  errorItem = undefined
  transformElement (r,s)= id

instance Target  Eletric [DotStatement Int] where
  renderNode = renderElemMecha
  renderLink = renderLinkMecha

instance Target  Thermal [DotStatement Int] where
  renderNode _ (_,(_,(ni,ThermalNode )))= [DN $ DotNode ni [shape Egg]]
  renderNode _ (_,(_,(ni,(Ambient v) )))= [DN $ DotNode ni [XLabel $  StrLabel $ T.pack $ (show v) <> "ºC" , shape MSquare]]
  renderLink (f,nf) (h,t) nis ni (HeatFlow v ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $ show v <> "F"] ]
  renderLink (f,nf) (h,t) nis ni (Conductor v ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $ show v <> "R"] ]


