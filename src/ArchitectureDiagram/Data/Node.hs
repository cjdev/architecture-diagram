{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Node
 ( Shape(..)
 , NodeStyle(..)
 , Node(..)
 ) where

import Data.Text (Text)
import Data.List (intercalate)

data Shape
  = Record
  | Box3d
  deriving (Show, Eq)

data NodeStyle
  = Rounded
  deriving (Show, Eq)

data Node = Node
  { _nName :: Text
  , _nShape :: Shape
  , _nStyles :: [NodeStyle]
  , _nWidth :: Maybe Float
  , _nChildren :: [Node]
  } deriving (Show, Eq)
