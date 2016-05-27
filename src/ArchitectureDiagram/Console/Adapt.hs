{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.Adapt
  ( toDataGraph
  , toDataNodes
  , toDataEdge
  ) where

import qualified Data.Map as Map
import Data.Text (Text)

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import qualified ArchitectureDiagram.Data.Edge as Data
import ArchitectureDiagram.Console.Types

toDataGraph :: Graph -> Data.Graph
toDataGraph g = Data.Graph (_gName g) (toDataNodes $ _gNodes g) (map toDataEdge $ _gEdges g)

toDataNodes :: Nodes -> [Data.Node]
toDataNodes = map toDataNode . Map.toList

toDataNode :: (Text, Node) -> Data.Node
toDataNode (ref, node) = Data.Node (_nName node) Data.Record [] Nothing []

toDataEdge :: Edge -> Data.Edge
toDataEdge e = Data.Edge [] (_eFrom e) (_eTo e) Data.From
