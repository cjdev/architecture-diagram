module ArchitectureDiagram.Source.Json.Adapt
  ( toDataGraph
  , toDataNodes
  , toDataEdge
  ) where

import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Default
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import qualified ArchitectureDiagram.Data.Edge as Data
import ArchitectureDiagram.Source.Json.Types

toDataGraph :: Graph -> Data.Graph
toDataGraph g = Data.Graph (_gName g) (toDataNodes $ _gNodes g) (map toDataEdge $ _gEdges g)

toDataNodes :: Nodes -> Map Data.NodeRef Data.Node
toDataNodes = Map.mapKeys Data.NodeRef . Map.mapWithKey toDataNode

toDataNode :: Text -> Node -> Data.Node
toDataNode ref node = Data.Node (fromMaybe ref (_nName node)) Data.Record [] Nothing Map.empty

toDataEdge :: Edge -> Data.Edge
toDataEdge e = Data.Edge [] (_eFrom e) (_eTo e) Data.From
