{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module ArchitectureDiagram.Console.Types
  ( Node(..)
  , Edge(..)
  , Graph(..)
  ) where

import GHC.Generics
import Data.Map (Map)
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH

import ArchitectureDiagram.Aeson (dropPrefixOptions)

data Node = Node
  { _nName :: Text
  , _nChildren :: Maybe Nodes
  } deriving (Show, Eq, Generic)

type Nodes = Map Text Node

data Edge = Edge
  deriving (Show, Eq)

type Edges = Map Text Edge

data Graph = Graph
  { _gNodes :: Nodes
  } deriving (Show, Eq, Generic)

$(deriveToJSON (dropPrefixOptions "_n") ''Node)
$(deriveFromJSON (dropPrefixOptions "_n") ''Node)

$(deriveToJSON (dropPrefixOptions "_g") ''Graph)
$(deriveFromJSON (dropPrefixOptions "_g") ''Graph)
