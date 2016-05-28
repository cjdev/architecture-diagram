module ArchitectureDiagram.Source.Json.Aeson
  ( dropPrefixOptions
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)

dropPrefixOptions :: String -> Options
dropPrefixOptions prefix = defaultOptions { fieldLabelModifier = firstToLower . drop (length prefix) }

firstToLower :: String -> String
firstToLower (x:xs) = toLower x : xs
firstToLower [] = []

