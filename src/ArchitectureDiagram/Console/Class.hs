module ArchitectureDiagram.Console.Class
  ( Console(..)
  ) where

class Console m where
  generateGraphvizDotFile :: FilePath -> FilePath -> m ()
