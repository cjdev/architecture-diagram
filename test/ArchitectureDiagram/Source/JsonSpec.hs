{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Source.JsonSpec (spec) where

import Test.Hspec

import qualified Data.Map as Map

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import qualified ArchitectureDiagram.Data.Edge as Data
import ArchitectureDiagram.Source.Json.Types
import ArchitectureDiagram.Source.Json.Adapt

baseNodeTypes :: Map.Map Data.NodeTypeRef Data.NodeType
baseNodeTypes = Map.fromList
  [ (Data.NodeTypeRef "type_a", Data.NodeType {
       Data._ntStyles = [Data.Rounded]
    })
  ]

spec :: Spec
spec = do
  describe "toDataNodes" $ do
    it "should take an empty map and return an empty list" $ do
      let actual = toDataNodes Map.empty Map.empty
      let expected = Map.empty
      actual `shouldBe` expected

    it "should take a singleton map and return a singleton list of the adapted node (Nothing Nothing Nothing)" $ do
      let actual = toDataNodes Map.empty $ Map.singleton "node_a" (Node Nothing Nothing Nothing)
      let expected = Map.fromList [("node_a", Data.Node "node_a" Data.Record [] Nothing Map.empty)]
      actual `shouldBe` expected

    it "should take a singleton map and return a singleton list of the adapted node (Nothing Just Nothing)" $ do
      let actual = toDataNodes Map.empty $ Map.singleton "node_a" (Node Nothing (Just "a") Nothing)
      let expected = Map.fromList [("node_a", Data.Node "a" Data.Record [] Nothing Map.empty)]
      actual `shouldBe` expected

    it "should take a singleton map and return a singleton list of the adapted node (Nothing Just Just)" $ do
      let actual = toDataNodes Map.empty $ Map.singleton "node_a" (Node Nothing (Just "a") (Just Map.empty))
      let expected = Map.fromList [("node_a", Data.Node "a" Data.Record [] Nothing Map.empty)]
      actual `shouldBe` expected

    it "should convert a node using its type" $ do
      let nodeA = ("node_a", Node (Just "type_a") (Just "a") (Just Map.empty))
      let actual = toDataNodes baseNodeTypes $ Map.fromList [nodeA]
      let expected = Map.fromList [("node_a", Data.Node "a" Data.Record [Data.Rounded] Nothing Map.empty)]
      actual `shouldBe` expected

    it "should try convert a node using its type but fail to find the type and use the defaults instead" $ do
      let nodeA = ("node_a", Node (Just "type_b") (Just "a") (Just Map.empty))
      let actual = toDataNodes baseNodeTypes $ Map.fromList [nodeA]
      let expected = Map.fromList [("node_a", Data.Node "a" Data.Record [] Nothing Map.empty)]
      actual `shouldBe` expected

    it "should convert a node its child" $ do
      let nodeB = ("node_b", Node Nothing (Just "b") (Just Map.empty))
      let nodeA = ("node_a", Node Nothing (Just "a") (Just $ Map.fromList [nodeB]))
      let actual = toDataNodes baseNodeTypes $ Map.fromList [nodeA]
      let expected = Map.fromList [("node_a", Data.Node "a" Data.Record [] Nothing (Map.fromList [("node_b", Data.Node "b" Data.Record [] Nothing Map.empty )]))]
      actual `shouldBe` expected

  describe "toDataEdge" $ do
    it "should take an console edge and return an adpated edge" $ do
      let actual = toDataEdge $ Edge "node_a" "node_b"
      let expected = Data.Edge [] "node_a" "node_b" Data.From
      actual `shouldBe` expected
