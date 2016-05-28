{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console
  ( stub
  ) where

import Language.Dot.Pretty (renderDot)

import ArchitectureDiagram.Data.Node
import ArchitectureDiagram.Data.Edge
import ArchitectureDiagram.Data.Graph
import ArchitectureDiagram.Target.Dot

stub :: String
stub = renderDot (toGraph $ Graph "AWS" nodes edges)
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
