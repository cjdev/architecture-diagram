{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.Impl.AbstractSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture

import qualified Data.ByteString.Lazy as BL
import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.FileManager.Class
import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.Console.Impl.Abstract

data Fixture m = Fixture
  -- FileManager
  { _readFile :: FilePath -> m BL.ByteString
  , _writeFile :: FilePath -> BL.ByteString -> m ()
  }

instance Monoid w => FileManager (TestFixture Fixture w s) where
  readFile = arg1 _readFile
  writeFile = arg2 _writeFile

base = Fixture
  { _readFile = \_ -> return BL.empty
  , _writeFile = \_ _ -> return ()
  }

spec :: Spec
spec = do
  describe "generateGraphvizDotFile" $ do
    it "should read the inbound file" $ do
      let actual = logTestFixture (generateGraphvizDotFile "inbound" "outbound") captureInboundFilePath ()
      let expected = ["inbound"]
      actual `shouldBe` expected

captureInboundFilePath :: Fixture (WS [FilePath] ())
captureInboundFilePath = base
  { _readFile = \inboundFilePath -> do
      tell [inboundFilePath]
      (_readFile base) inboundFilePath
  }
