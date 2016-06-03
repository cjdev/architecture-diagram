module ArchitectureDiagram.Source.Json.Adapt
  ( toDataGraph
  , toDataNodes
  , toDataNodeTypes
  , toDataEdge
  , toDataEdgeTypes
  ) where

import qualified Data.Map as Map
import Control.Monad (join)
import Control.Applicative ((<|>))
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
  edgeTypes = toDataEdgeTypes (_gEdgeTypes g)
  in Data.Graph
    { Data._gName = _gName g
    , Data._gNodes = toDataNodes nodeTypes (_gNodes g)
    , Data._gEdges = map (toDataEdge edgeTypes) (_gEdges g)
    }

toDataNodeTypes :: NodeTypes -> Map Data.NodeTypeRef Data.NodeType
toDataNodeTypes = Map.mapKeys Data.NodeTypeRef . Map.map toDataNodeType

toDataNodeType :: NodeType -> Data.NodeType
toDataNodeType nt = Data.NodeType
  { Data._ntName = _ntName nt
  , Data._ntStyles = fromMaybe [] (_ntStyles nt)
  }

toDataNodes :: Map Data.NodeTypeRef Data.NodeType -> Nodes -> Map Data.NodeRef Data.Node
toDataNodes types = Map.mapKeys Data.NodeRef . Map.mapWithKey (toDataNode types)

toDataNode :: Map Data.NodeTypeRef Data.NodeType -> Text -> Node -> Data.Node
toDataNode types ref node = Data.Node
  { Data._nName = fromMaybe ref (_nName node <|> join (fromType Data._ntName))
  , Data._nShape = Data.Record
  , Data._nStyles = fromMaybe [] (fromType Data._ntStyles)
  , Data._nWidth = Nothing
  , Data._nNodes = fromMaybe Map.empty (toDataNodes types <$> _nNodes node)
  }
  where
    fromType :: (Data.NodeType -> a) -> Maybe a
    fromType access = do
      ty <- _nType node
      nt <- Map.lookup (Data.NodeTypeRef ty) types
      return $ access nt

toDataEdge :: Map Data.EdgeTypeRef Data.EdgeType -> Edge -> Data.Edge
toDataEdge types edge = Data.Edge
  { Data._eFrom = _eFrom edge
  , Data._eTo = _eTo edge
  , Data._eRank = Data.From
  , Data._eStyles = fromMaybe [] (fromType Data._etStyles)
  }
  where
    fromType :: (Data.EdgeType -> a) -> Maybe a
    fromType access = do
      ty <- _eType edge
      et <- Map.lookup ty types
      return $ access et


toDataEdgeTypes :: EdgeTypes -> Map Data.EdgeTypeRef Data.EdgeType
toDataEdgeTypes = Map.mapKeys Data.EdgeTypeRef . Map.map toDataEdgeType

toDataEdgeType :: EdgeType -> Data.EdgeType
toDataEdgeType et = Data.EdgeType
  { Data._etStyles = fromMaybe [] (_etStyles et)
  }
