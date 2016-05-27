{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.Adapt
  ( toDataGraph
  , toDataNodes
  ) where

import qualified Data.Map as Map
import Data.Text (Text)

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import ArchitectureDiagram.Console.Types

toDataGraph :: Graph -> Data.Graph
toDataGraph g = Data.Graph (_gName g) (toDataNodes $ _gNodes g)

toDataNodes :: Nodes -> [Data.Node]
toDataNodes = map toDataNode . Map.toList

toDataNode :: (Text, Node) -> Data.Node
toDataNode (ref, node) = Data.Node (_nName node) Data.Record [] Nothing []
