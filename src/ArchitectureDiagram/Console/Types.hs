{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module ArchitectureDiagram.Console.Types
  ( Node(..)
  , Edge(..)
  , Graph(..)
  ) where

import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Default
import Data.Map (Map)
import Data.Text (Text)

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

instance Default Graph where
  def = Graph Map.empty

$(deriveToJSON (dropPrefixOptions "_n") ''Node)
$(deriveFromJSON (dropPrefixOptions "_n") ''Node)

$(deriveToJSON (dropPrefixOptions "_g") ''Graph)
$(deriveFromJSON (dropPrefixOptions "_g") ''Graph)
