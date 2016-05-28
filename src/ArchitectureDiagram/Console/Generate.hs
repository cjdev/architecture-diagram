{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.Generate
  ( generate
  ) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error.Class
import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Text.Conversions (convertText, UTF8(..))
import Language.Dot.Pretty (renderDot)

import ArchitectureDiagram.Console.Class
import ArchitectureDiagram.Source.Json
import ArchitectureDiagram.Target.Dot (toGraph)

generate :: Console m => m ()
generate = do
  contents <- readStdin
  case decode contents of
    Nothing -> do
      writeStderr "Unable to parse input\n"
      failure
    Just graph -> do
      let dotGraph = toGraph $ toDataGraph graph
      let (UTF8 bytes) = convertText $ renderDot dotGraph
      writeStdout bytes
