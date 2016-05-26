module ArchitectureDiagram.Console.Class
  ( Console(..)
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

class Monad m => Console m where
  readStdin :: m BL.ByteString
  writeStdout :: BL.ByteString -> m ()
  writeStderr :: Text -> m ()
  failure :: m ()
