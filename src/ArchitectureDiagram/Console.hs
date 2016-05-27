{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console
  ( stub
  ) where

import Language.Dot.Pretty (renderDot)

import ArchitectureDiagram.Data.ToStatement
import ArchitectureDiagram.Data.Node
import ArchitectureDiagram.Data.Edge
import ArchitectureDiagram.Data.Graph

stub :: String
stub = renderDot (toDotGraph $ Graph "AWS" nodes edges)
  where
    nodes :: [Node]
    nodes =
      [ Node "NodeA" Box3d [] Nothing []
      , Node "ClusterA" Record [] Nothing
          [ Node "NodeB" Record [Rounded] Nothing []
          , Node "NodeC" Record [Rounded] Nothing []
          ]
      ]
    edges :: [Edge]
    edges =
      [ Edge [] "NodeA" "NodeB" From
      ]
