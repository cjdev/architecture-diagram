{-# LANGUAGE FlexibleContexts #-}
module ArchitectureDiagram.Console.Generate
  ( generate
  ) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error.Class

import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.Console.Class

generate :: Console m => m ()
generate = do
  contents <- readStdin
  return ()
