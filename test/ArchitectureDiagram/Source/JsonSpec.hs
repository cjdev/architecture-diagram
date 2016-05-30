{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Source.JsonSpec (spec) where

import Test.Hspec

import qualified Data.Map as Map

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import qualified ArchitectureDiagram.Data.Edge as Data
import ArchitectureDiagram.Source.Json

spec :: Spec
spec = do
  describe "toDataNodes" $ do
    it "should take an empty map and return an empty list" $ do
      let actual = toDataNodes Map.empty
      let expected = []
      actual `shouldBe` expected
    it "should take a singleton map and return a singleton list of the adapted node (Nothing Nothing)" $ do
      let actual = toDataNodes $ Map.singleton "node_a" (Node Nothing Nothing)
      let expected = [Data.Node "node_a" "node_a" Data.Record [] Nothing []]
      actual `shouldBe` expected

    it "should take a singleton map and return a singleton list of the adapted node (Just Nothing)" $ do
      let actual = toDataNodes $ Map.singleton "node_a" (Node (Just "a") Nothing)
      let expected = [Data.Node "node_a" "a" Data.Record [] Nothing []]
      actual `shouldBe` expected

    it "should take a singleton map and return a singleton list of the adapted node (Just Just)" $ do
      let actual = toDataNodes $ Map.singleton "node_a" (Node (Just "a") (Just Map.empty))
      let expected = [Data.Node "node_a" "a" Data.Record [] Nothing []]
      actual `shouldBe` expected

  describe "toDataEdge" $ do
    it "should take an console edge and return an adpated edge" $ do
      let actual = toDataEdge $ Edge "node_a" "node_b"
      let expected = Data.Edge [] "node_a" "node_b" Data.From
      actual `shouldBe` expected
