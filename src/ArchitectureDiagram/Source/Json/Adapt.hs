module ArchitectureDiagram.Source.Json.Adapt
  ( toDataGraph
  , toDataNodes
  , toDataNodeTypes
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
toDataGraph g = Data.Graph
  { Data._gName = _gName g
  , Data._gNodes = toDataNodes (_gNodes g)
  , Data._gEdges = map toDataEdge (_gEdges g)
  , Data._gNodeTypes = toDataNodeTypes (_gNodeTypes g)
  }

toDataNodeTypes :: NodeTypes -> Map Data.NodeTypeRef Data.NodeType
toDataNodeTypes = Map.mapKeys Data.NodeTypeRef . Map.map toDataNodeType

toDataNodeType :: NodeType -> Data.NodeType
toDataNodeType nt = Data.NodeType
  { Data._ntStyles = fromMaybe [] (_ntStyles nt)
  }

toDataNodes :: Nodes -> Map Data.NodeRef Data.Node
toDataNodes = Map.mapKeys Data.NodeRef . Map.mapWithKey toDataNode

toDataNode :: Text -> Node -> Data.Node
toDataNode ref node = Data.Node (fromMaybe ref (_nName node)) Data.Record [] Nothing Map.empty

toDataEdge :: Edge -> Data.Edge
toDataEdge e = Data.Edge [] (_eFrom e) (_eTo e) Data.From
