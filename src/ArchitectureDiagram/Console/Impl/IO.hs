module ArchitectureDiagram.Console.Impl.IO
  ( readStdin
  , writeStdout
  , writeStderr
  , failure
  ) where

import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import Data.Text (Text)
import System.IO (stdin, stdout, stderr)

readStdin :: IO BL.ByteString
readStdin = BL.hGetContents stdin

writeStdout :: BL.ByteString -> IO ()
writeStdout = BL.hPutStr stdout

writeStderr :: Text -> IO ()
writeStderr = T.hPutStr stderr

failure :: IO ()
failure = exitFailure
