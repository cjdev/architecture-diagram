{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Edge
 ( Edge(..)
 , EdgeStyle(..)
 , EdgeRank(..)
 ) where

import Language.Dot.Syntax
import Data.Text (Text)
import Data.Text.Conversions (fromText)

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
