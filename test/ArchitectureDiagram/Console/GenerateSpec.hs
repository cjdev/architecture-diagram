{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.GenerateSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture

import qualified Data.ByteString.Lazy as BL
import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Console.Class
import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.Console.Generate

data Fixture m = Fixture
  -- FileManager
  { _readStdin:: m BL.ByteString
  , _writeStdout :: BL.ByteString -> m ()
  }

instance Monoid w => Console (TestFixture Fixture w s) where
  readStdin = arg0 _readStdin
  writeStdout = arg1 _writeStdout

base :: Monoid w => Fixture (WS w s)
base = Fixture
  { _readStdin = return BL.empty
  , _writeStdout = \_ -> return ()
  }

spec :: Spec
spec = do
  describe "generate" $ do
    it "should read stdin" $ do
      let captureStdin = base
            { _readStdin = do
                tell ()
                _readStdin base
            }
      let actual = logTestFixture generate captureStdin ()
      let expected = ()
      actual `shouldBe` expected
