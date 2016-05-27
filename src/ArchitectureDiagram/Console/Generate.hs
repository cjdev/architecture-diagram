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

import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.Console.Class
import ArchitectureDiagram.Console.Adapt
import ArchitectureDiagram.Data.Graph (toDotGraph)


generate :: Console m => m ()
generate = do
  contents <- readStdin
  case decode contents of
    Nothing -> do
      writeStderr "unable to parse input"
      failure
    Just graph -> do
      let dotGraph = toDotGraph $ toDataGraph graph
      let (UTF8 bytes) = convertText $ renderDot dotGraph
      writeStdout bytes
