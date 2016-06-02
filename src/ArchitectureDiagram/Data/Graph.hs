{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Graph
  ( Graph(..)
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Text.Conversions (fromText)

import ArchitectureDiagram.Data.Node (Node, NodeRef, NodeType, NodeTypeRef)
import ArchitectureDiagram.Data.Edge (Edge, EdgeTypeRef, EdgeType)

data Graph = Graph
  { _gName :: Text
  , _gNodes :: Map NodeRef Node
  , _gEdges :: [Edge]
  } deriving (Show, Eq)
