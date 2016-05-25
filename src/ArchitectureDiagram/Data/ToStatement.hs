{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.ToStatement
 ( ToStatement(..)
 ) where

import Language.Dot.Syntax (Statement)

class ToStatement a where
  toStatement :: a -> Statement
