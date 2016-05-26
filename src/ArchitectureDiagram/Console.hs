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
stub = renderDot (toGraph $ Graph "AWS" nodes)
  where
    nodes :: [Node]
    nodes =
      [ Node "LoadBalancer" Box3d [] Nothing []
      , Node "Proxy" Record [] Nothing
          [ Node "NginxProxy" Record [Rounded] Nothing []
          , Node "HelloResource" Record [Rounded] Nothing []
          ]
      ]
