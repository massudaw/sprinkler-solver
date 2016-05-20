{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,GADTs,TypeFamilies, FlexibleContexts,RankNTypes,TupleSections,RecursiveDo, NoMonomorphismRestriction #-}
module Backend.Graphviz (renderGraph ) where

import Eletric
import Thermal
import Domains
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
         , graphStatements = Seq.fromList  i}) Png f

renderElemMecha  ni Node  = [DN $ DotNode ni [shape MDiamond]]
renderElemMecha  ni Ground  = [DN $ DotNode ni [shape MSquare]]
renderElemMecha  ni i = error $ show (ni,i)

-- renderLinkMecha  nis ni (VoltageSource v ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $ show v <> "V"] ]
-- renderLinkMecha  nis ni (Resistor r ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $show r <> "ohm" ] ]

instance RBackend [DotStatement Int] where
  type TCoord [DotStatement Int]= V3 Double
  errorItem = undefined
  transformElement (r,s)= id

instance Target  Eletric [DotStatement Int] where
  renderNode = renderElemMecha
  -- renderLink = renderLinkMecha

instance Target  Thermal [DotStatement Int] where
  renderNode  ni ThermalNode = [DN $ DotNode ni [shape Egg]]
  renderNode  ni (Ambient v) = [DN $ DotNode ni [XLabel $  StrLabel $ T.pack $ (show v) <> "ºC" , shape MDiamond]]
  -- renderLink  nis ni (HeatFlow v ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $ show v <> "F"] ]
--  renderLink  nis ni (Conductor v ) = [DE $ DotEdge h t [Label $ StrLabel $ T.pack $ show v <> "R"] ]


