{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Source.Json.Types
  ( Node(..)
  , Nodes
  , NodeType(..)
  , NodeTypes
  , Edge(..)
  , Edges
  , EdgeType(..)
  , EdgeTypes
  , Graph(..)
  ) where

import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Default
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified ArchitectureDiagram.Data.Node as Data
import qualified ArchitectureDiagram.Data.Edge as Data
import ArchitectureDiagram.Source.Json.Aeson (dropPrefixOptions)

data Node = Node
  { _nType :: Maybe Text
  , _nName :: Maybe Text
  , _nNodes :: Maybe Nodes
  } deriving (Show, Eq, Generic)

instance Default Node where
  def = Node
    { _nType = Nothing
    , _nName = Nothing
    , _nNodes = Nothing
    }

type Nodes = Map Text Node

data NodeType = NodeType
  { _ntStyles :: Maybe [Data.NodeStyle]
  , _ntName :: Maybe Text
  } deriving (Show, Eq, Generic)

instance Default NodeType where
  def = NodeType
    { _ntStyles = Nothing
    , _ntName = Nothing
    }

type NodeTypes = Map Text NodeType

data Edge = Edge
  { _eType :: Maybe Data.EdgeTypeRef
  , _eFrom :: Data.NodeRef
  , _eTo :: Data.NodeRef
  } deriving (Show, Eq, Generic)

type Edges = [Edge]

data EdgeType = EdgeType
  { _etStyles :: Maybe [Data.EdgeStyle]
  } deriving (Show, Eq, Generic)

instance Default EdgeType where
  def = EdgeType
    { _etStyles = Nothing
    }

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
