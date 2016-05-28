{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Graph
  ( Graph(..)
  ) where

import Data.Text (Text)
import Data.Text.Conversions (fromText)

import ArchitectureDiagram.Data.Node (Node)
import ArchitectureDiagram.Data.Edge (Edge)

data Graph = Graph
  { _gName :: Text
  , _gNodes :: [Node]
  , _gEdges :: [Edge]
  } deriving (Show, Eq)
