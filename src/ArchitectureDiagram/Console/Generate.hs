{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.Generate
  ( generate
  ) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error.Class
import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Yaml (decodeEither)
import Data.Text.Conversions (convertText, UTF8(..), toText)
import Language.Dot.Pretty (renderDot)

import ArchitectureDiagram.Console.Class
import ArchitectureDiagram.Source.Json.Adapt
import ArchitectureDiagram.Target.Dot (toGraph)

generate :: Console m => m ()
generate = do
  contents <- readStdin
  case decodeEither (BL.toStrict contents) of
    Left str -> do
      writeStderr $ toText str
      failure
    Right graph -> do
      let dotGraph = toGraph $ toDataGraph graph
      let (UTF8 bytes) = convertText $ renderDot dotGraph
      writeStdout bytes
