{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Source.Json
  ( Node(..)
  , Nodes
  , NodeType(..)
  , NodeTypes
  , Edge(..)
  , Edges
  , EdgeType(..)
  , EdgeTypes
  , Graph(..)
  , toDataGraph
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
import ArchitectureDiagram.Source.Json.Aeson (dropPrefixOptions)

data Node = Node
  { _nName :: Maybe Text
  , _nChildren :: Maybe Nodes
  } deriving (Show, Eq, Generic)

type Nodes = Map Text Node

data NodeType = NodeType
  deriving (Show, Eq, Generic)

type NodeTypes = Map Text NodeType

data Edge = Edge
  { _eFrom :: Text
  , _eTo :: Text
  } deriving (Show, Eq, Generic)

type Edges = [Edge]

data EdgeType = EdgeType
  deriving (Show, Eq, Generic)

type EdgeTypes = Map Text EdgeType

data Graph = Graph
  { _gName :: Text
  , _gNodes :: Nodes
  , _gNodeTypes :: NodeTypes
  , _gEdges :: Edges
  , _gEdgeTypes :: EdgeTypes
  } deriving (Show, Eq, Generic)

instance Default Graph where
  def = Graph "default" Map.empty Map.empty [] Map.empty

toDataGraph :: Graph -> Data.Graph
toDataGraph g = Data.Graph (_gName g) (toDataNodes $ _gNodes g) (map toDataEdge $ _gEdges g)

toDataNodes :: Nodes -> Map Data.NodeRef Data.Node
toDataNodes = Map.mapKeys Data.NodeRef . Map.mapWithKey toDataNode

toDataNode :: Text -> Node -> Data.Node
toDataNode ref node = Data.Node (fromMaybe ref (_nName node)) Data.Record [] Nothing Map.empty

toDataEdge :: Edge -> Data.Edge
toDataEdge e = Data.Edge [] (_eFrom e) (_eTo e) Data.From

$(deriveToJSON (dropPrefixOptions "_n") ''Node)
$(deriveFromJSON (dropPrefixOptions "_n") ''Node)

$(deriveToJSON (dropPrefixOptions "_nt") ''NodeType)
$(deriveFromJSON (dropPrefixOptions "_nt") ''NodeType)

$(deriveToJSON (dropPrefixOptions "_e") ''Edge)
$(deriveFromJSON (dropPrefixOptions "_e") ''Edge)

$(deriveToJSON (dropPrefixOptions "_et") ''EdgeType)
$(deriveFromJSON (dropPrefixOptions "_et") ''EdgeType)

$(deriveToJSON (dropPrefixOptions "_g") ''Graph)
$(deriveFromJSON (dropPrefixOptions "_g") ''Graph)
