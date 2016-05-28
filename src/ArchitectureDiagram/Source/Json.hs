{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Source.Json
  ( Node(..)
  , Nodes
  , Edge(..)
  , Edges
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
import Data.Text (Text)

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import qualified ArchitectureDiagram.Data.Edge as Data
import ArchitectureDiagram.Source.Json.Aeson (dropPrefixOptions)

data Node = Node
  { _nName :: Text
  , _nChildren :: Maybe Nodes
  } deriving (Show, Eq, Generic)

type Nodes = Map Text Node

data Edge = Edge
  { _eFrom :: Text
  , _eTo :: Text
  } deriving (Show, Eq, Generic)

type Edges = [Edge]

data Graph = Graph
  { _gName :: Text
  , _gNodes :: Nodes
  , _gEdges :: Edges
  } deriving (Show, Eq, Generic)

instance Default Graph where
  def = Graph "default" Map.empty []

toDataGraph :: Graph -> Data.Graph
toDataGraph g = Data.Graph (_gName g) (toDataNodes $ _gNodes g) (map toDataEdge $ _gEdges g)

toDataNodes :: Nodes -> [Data.Node]
toDataNodes = map toDataNode . Map.toList

toDataNode :: (Text, Node) -> Data.Node
toDataNode (ref, node) = Data.Node ref (_nName node) Data.Record [] Nothing []

toDataEdge :: Edge -> Data.Edge
toDataEdge e = Data.Edge [] (_eFrom e) (_eTo e) Data.From

$(deriveToJSON (dropPrefixOptions "_n") ''Node)
$(deriveFromJSON (dropPrefixOptions "_n") ''Node)

$(deriveToJSON (dropPrefixOptions "_e") ''Edge)
$(deriveFromJSON (dropPrefixOptions "_e") ''Edge)

$(deriveToJSON (dropPrefixOptions "_g") ''Graph)
$(deriveFromJSON (dropPrefixOptions "_g") ''Graph)
