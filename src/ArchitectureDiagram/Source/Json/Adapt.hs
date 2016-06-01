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
toDataGraph g = let
  nodeTypes = toDataNodeTypes (_gNodeTypes g)
  in Data.Graph
    { Data._gName = _gName g
    , Data._gNodes = toDataNodes nodeTypes (_gNodes g)
    , Data._gEdges = map toDataEdge (_gEdges g)
    , Data._gNodeTypes = nodeTypes
    }

toDataNodeTypes :: NodeTypes -> Map Data.NodeTypeRef Data.NodeType
toDataNodeTypes = Map.mapKeys Data.NodeTypeRef . Map.map toDataNodeType

toDataNodeType :: NodeType -> Data.NodeType
toDataNodeType nt = Data.NodeType
  { Data._ntStyles = fromMaybe [] (_ntStyles nt)
  }

toDataNodes :: Map Data.NodeTypeRef Data.NodeType -> Nodes -> Map Data.NodeRef Data.Node
toDataNodes types = Map.mapKeys Data.NodeRef . Map.mapWithKey (toDataNode types)

toDataNode :: Map Data.NodeTypeRef Data.NodeType -> Text -> Node -> Data.Node
toDataNode types ref node = Data.Node
  { Data._nName = fromMaybe ref (_nName node)
  , Data._nShape = Data.Record
  , Data._nStyles = fromMaybe [] (fromType Data._ntStyles)
  , Data._nWidth = Nothing
  , Data._nChildren = Map.empty
  }
  where
    fromType :: (Data.NodeType -> a) -> Maybe a
    fromType access = do
      ty <- _nType node
      nt <- Map.lookup (Data.NodeTypeRef ty) types
      return $ access nt

toDataEdge :: Edge -> Data.Edge
toDataEdge e = Data.Edge
  { Data._eStyles = []
  , Data._eFrom = _eFrom e
  , Data._eTo = _eTo e
  , Data._eRank = Data.From
  }
