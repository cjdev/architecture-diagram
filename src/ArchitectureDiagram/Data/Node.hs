{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ArchitectureDiagram.Data.Node
 ( Shape(..)
 , NodeStyle(..)
 , Node(..)
 , NodeRef(..)
 , NodeType(..)
 , NodeTypeRef(..)
 ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.TH
import Data.Text (Text)
import Data.Map (Map)
import Data.List (intercalate)
import Data.String (IsString)

import ArchitectureDiagram.Source.Json.Aeson (firstToLowerOptions)

data Shape
  = Record
  | Box3d
  deriving (Show, Eq)

data NodeStyle
  = Rounded
  deriving (Show, Eq)

newtype NodeRef = NodeRef { unNodeRef :: Text }
  deriving (Show, Eq, Ord, IsString, ToJSON, FromJSON)

newtype NodeTypeRef = NodeTypeRef { unNodeTypeRef :: Text }
  deriving (Show, Eq, Ord, IsString, ToJSON, FromJSON)

data Node = Node
  { _nName :: Text
  , _nShape :: Shape
  , _nStyles :: [NodeStyle]
  , _nWidth :: Maybe Float
  , _nChildren :: Map NodeRef Node
  } deriving (Show, Eq)

data NodeType = NodeType
  { _ntStyles :: [NodeStyle]
  } deriving (Show, Eq)

$(deriveToJSON firstToLowerOptions ''Shape)
$(deriveFromJSON firstToLowerOptions ''Shape)

$(deriveToJSON firstToLowerOptions ''NodeStyle)
$(deriveFromJSON firstToLowerOptions ''NodeStyle)
