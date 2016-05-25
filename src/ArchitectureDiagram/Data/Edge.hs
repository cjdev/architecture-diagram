{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Edge
 ( Edge(..)
 , EdgeStyle(..)
 , EdgeRank(..)
 ) where

import Data.Text (Text)

data EdgeStyle
  = Dashed
  deriving (Show, Eq)

data EdgeRank
  = To
  | From
  deriving (Show, Eq)

data Edge = Edge
  { _eStyles :: [EdgeStyle]
  , _eTo :: Text
  , _eFrom :: Text
  , _eRank :: EdgeRank
  } deriving (Show, Eq)
