{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Console.AdaptSpec (spec) where

import Test.Hspec

import qualified Data.Map as Map

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import ArchitectureDiagram.Console.Types
import ArchitectureDiagram.Console.Adapt

spec :: Spec
spec = do
  describe "toDataNodes" $ do
    it "should take an empty map and return an empty list" $ do
      let actual = toDataNodes Map.empty
      let expected = []
      actual `shouldBe` expected
    it "should take a singleton map and return a singleton list of the adapted node (Nothing)" $ do
      let actual = toDataNodes $ Map.singleton "key" (Node "node_a" Nothing)
      let expected = [Data.Node "node_a" Data.Record [] Nothing []]
      actual `shouldBe` expected

    it "should take a singleton map and return a singleton list of the adapted node (Just)" $ do
      let actual = toDataNodes $ Map.singleton "key" (Node "node_a" (Just Map.empty))
      let expected = [Data.Node "node_a" Data.Record [] Nothing []]
      actual `shouldBe` expected