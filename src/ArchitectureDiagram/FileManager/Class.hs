module ArchitectureDiagram.FileManager.Class
  ( FileManager(..)
  ) where

import qualified Data.ByteString.Lazy as BL

class Monad m => FileManager m where
  readFile :: FilePath -> m BL.ByteString
  writeFile :: FilePath -> BL.ByteString -> m ()
