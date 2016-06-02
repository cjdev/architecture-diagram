{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ArchitectureDiagram.Data.Edge
 ( Edge(..)
 , EdgeStyle(..)
 , EdgeType(..)
 , EdgeTypeRef(..)
 , EdgeRank(..)
 ) where

import Control.Monad (mzero)
import Data.String (IsString)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Text (Text)
import Data.Text.Conversions (fromText)

import ArchitectureDiagram.Data.Node (NodeRef)

data EdgeStyle
  = Dashed
  deriving (Show, Eq)

instance ToJSON EdgeStyle where
  toJSON Dashed = String "dashed"

instance FromJSON EdgeStyle where
  parseJSON (String "dashed") = pure Dashed
  parseJSON _ = mzero

data EdgeRank
  = To
  | From
  deriving (Show, Eq)

newtype EdgeTypeRef = EdgeTypeRef { unEdgeTypeRef :: Text }
  deriving (Show, Eq, Ord, IsString, ToJSON, FromJSON)

data Edge = Edge
  { _eStyles :: [EdgeStyle]
  , _eFrom :: NodeRef
  , _eTo :: NodeRef
  , _eRank :: EdgeRank
  } deriving (Show, Eq)

data EdgeType = EdgeType
  { _etStyles :: [EdgeStyle]
  } deriving (Show, Eq)
