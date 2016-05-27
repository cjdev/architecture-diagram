{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Edge
 ( Edge(..)
 , EdgeStyle(..)
 , EdgeRank(..)
 ) where

import Language.Dot.Syntax
import Data.Text (Text)
import Data.Text.Conversions (fromText)

import ArchitectureDiagram.Data.ToStatement (ToStatement(..))

data EdgeStyle
  = Dashed
  deriving (Show, Eq)

data EdgeRank
  = To
  | From
  deriving (Show, Eq)

data Edge = Edge
  { _eStyles :: [EdgeStyle]
  , _eFrom :: Text
  , _eTo :: Text
  , _eRank :: EdgeRank
  } deriving (Show, Eq)

instance ToStatement Edge where
  toStatement e = EdgeStatement
    [ ENodeId DirectedEdge (NodeId (StringId $ fromText (_eFrom e)) Nothing)
    , ENodeId NoEdge (NodeId (StringId $ fromText (_eTo e)) Nothing)
    ]
    []
