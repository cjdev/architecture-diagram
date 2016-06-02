{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wiring where

import qualified ArchitectureDiagram.Console.Impl.IO as Console
import ArchitectureDiagram.Console.Class

instance Console IO where
  readStdin = Console.readStdin
  writeStdout = Console.writeStdout
  writeStderr = Console.writeStderr
  failure = Console.failure
