{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.GenerateSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture

import qualified Data.ByteString.Lazy as BL
import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Data.Aeson (encode)
import Data.Default (def)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Console.Class
import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.Console.Generate

data Fixture m = Fixture
  -- Console
  { _readStdin:: m BL.ByteString
  , _writeStdout :: BL.ByteString -> m ()
  , _writeStderr :: Text -> m ()
  , _failure :: m ()
  }

instance Monoid w => Console (TestFixture Fixture w s) where
  readStdin = arg0 _readStdin
  writeStdout = arg1 _writeStdout
  writeStderr = arg1 _writeStderr
  failure = arg0 _failure

base :: Monoid w => Fixture (WS w s)
base = Fixture
  { _readStdin = return BL.empty
  , _writeStdout = \_ -> return ()
  , _writeStderr = \_ -> return ()
  , _failure = return ()
  }

captureWritesWithBadInput :: Fixture (WS [String] ())
captureWritesWithBadInput = base
  { _writeStderr = \err -> do
     tell ["stderr"]
     _writeStderr base err
  , _writeStdout = \txt -> do
     tell ["stdout"]
     _writeStdout base txt
  }

captureWritesWithGoodInput :: Fixture (WS [String] ())
captureWritesWithGoodInput = base
  { _readStdin = do
      return $ encode (def :: Graph)
  , _writeStderr = \err -> do
     tell ["stderr"]
     _writeStderr base err
  , _writeStdout = \txt -> do
     tell ["stdout"]
     _writeStdout base txt
  }

spec :: Spec
spec = do
  describe "generate" $ do
    it "should read stdin" $ do
      let captureStdin = base
            { _readStdin = do
                tell [()]
                _readStdin base
            }
      let actual = logTestFixture generate captureStdin ()
      let expected = [()]
      actual `shouldBe` expected
    it "should only write to stderr if can't parse" $ do
      let actual = logTestFixture generate captureWritesWithBadInput ()
      let expected = ["stderr"]
      actual `shouldBe` expected
    it "should only write to stdout if can parse" $ do
      let actual = logTestFixture generate captureWritesWithGoodInput ()
      let expected = ["stdout"]
      actual `shouldBe` expected
