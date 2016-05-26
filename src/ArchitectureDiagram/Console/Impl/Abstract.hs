{-# LANGUAGE FlexibleContexts #-}
module ArchitectureDiagram.Console.Impl.Abstract
  ( generateGraphvizDotFile
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Aeson (decode)

import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.FileManager.Class

generateGraphvizDotFile :: FileManager m => FilePath -> FilePath -> m ()
generateGraphvizDotFile inboundFilePath outboundFilePath = do
  contents <- readFile inboundFilePath
  return ()
