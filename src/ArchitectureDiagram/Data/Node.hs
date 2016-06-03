{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Node
 ( Shape(..)
 , NodeStyle(..)
 , Node(..)
 , NodeRef(..)
 , NodeType(..)
 , NodeTypeRef(..)
 ) where

import Control.Monad (mzero)
import Data.Default (Default(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
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

instance FromJSON Shape where
  parseJSON (String "record") = pure Record
  parseJSON (String "box3d") = pure Box3d
  parseJSON _ = mzero

instance ToJSON Shape where
  toJSON Record = String "record"
  toJSON Box3d = String "box3d"

data NodeStyle
  = Rounded
  deriving (Show, Eq)

instance FromJSON NodeStyle where 
  parseJSON (String "rounded") = pure Rounded
  parseJSON _ = mzero

instance ToJSON NodeStyle where
  toJSON Rounded = "rounded"

newtype NodeRef = NodeRef { unNodeRef :: Text }
  deriving (Show, Eq, Ord, IsString, ToJSON, FromJSON)

newtype NodeTypeRef = NodeTypeRef { unNodeTypeRef :: Text }
  deriving (Show, Eq, Ord, IsString, ToJSON, FromJSON)

data Node = Node
  { _nName :: Text
  , _nShape :: Shape
  , _nStyles :: [NodeStyle]
  , _nWidth :: Maybe Float
  , _nNodes :: Map NodeRef Node
  } deriving (Show, Eq)

data NodeType = NodeType
  { _ntName :: Maybe Text
  , _ntStyles :: [NodeStyle]
  } deriving (Show, Eq)

instance Default NodeType where
  def = NodeType
    { _ntName = Nothing
    , _ntStyles = []
    }
