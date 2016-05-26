{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.Generate
  ( generate
  ) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error.Class
import Data.Aeson (decode, encode)
import Data.Text (Text)

import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.Console.Class

generate :: Console m => m ()
generate = do
  contents <- readStdin
  maybe
    (writeStderr "unable to parse input" >> failure)
    (writeStdout . encode)
    (decode contents :: Maybe Graph)
