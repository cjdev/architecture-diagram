{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Edge
 ( Edge(..)
 , EdgeStyle(..)
 , EdgeRank(..)
 ) where

import Data.Text (Text)
import Data.Text.Conversions (fromText)

import ArchitectureDiagram.Data.Node (NodeRef)

data EdgeStyle
  = Dashed
  deriving (Show, Eq)

data EdgeRank
  = To
  | From
  deriving (Show, Eq)

data Edge = Edge
  { _eStyles :: [EdgeStyle]
  , _eFrom :: NodeRef
  , _eTo :: NodeRef
  , _eRank :: EdgeRank
  } deriving (Show, Eq)
