{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Node
 ( Shape(..)
 , NodeStyle(..)
 , Node(..)
 ) where

import Data.Text (Text)
import Data.Text.Conversions (ToText(..), convertText, fromText)
import Data.List (intercalate)

data Shape
  = Record
  | Box3d
  deriving (Show, Eq)

instance ToText Shape where
  toText Record = "record"
  toText Box3d = "box3d"

data NodeStyle
  = Rounded
  deriving (Show, Eq)

instance ToText NodeStyle where
  toText Rounded = "rounded"

data Node = Node
  { _nName :: Text
  , _nShape :: Shape
  , _nStyles :: [NodeStyle]
  , _nWidth :: Maybe Float
  , _nChildren :: [Node]
  } deriving (Show, Eq)
