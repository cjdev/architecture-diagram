{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Source.JsonSpec (spec) where

import Test.Hspec

import qualified Data.Map as Map
import Data.Default (def)

import qualified ArchitectureDiagram.Data.Graph as Data
import qualified ArchitectureDiagram.Data.Node as Data
import qualified ArchitectureDiagram.Data.Edge as Data
import ArchitectureDiagram.Source.Json.Types
import ArchitectureDiagram.Source.Json.Adapt

baseNodeTypes :: Map.Map Data.NodeTypeRef Data.NodeType
baseNodeTypes = Map.fromList
  [ ( "type_a"
    , Data.NodeType
       { Data._ntStyles = [Data.Rounded]
       , Data._ntName = Nothing
       }
    )
  ]

baseEdgeTypes :: Map.Map Data.EdgeTypeRef Data.EdgeType
baseEdgeTypes = Map.fromList
  [ ( "edge_a"
    , Data.EdgeType
        { Data._etStyles = [Data.Dashed]
        }
    )
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

    describe "should convert a node using its type" $ do
      it "should convert the styles" $ do
        let nodeA = ("node_a", Node (Just "type_a") (Just "a") (Just Map.empty))
        let actual = toDataNodes baseNodeTypes $ Map.fromList [nodeA]
        let expected = Map.fromList [("node_a", Data.Node "a" Data.Record [Data.Rounded] Nothing Map.empty)]
        actual `shouldBe` expected

      it "should convert the default name" $ do
        let nodeA = ("node_a", Node (Just "some_type") Nothing (Just Map.empty))
        let actual = toDataNodes (Map.singleton "some_type" (def { Data._ntName = Just "some name" })) (Map.fromList [nodeA])
        let expected = Map.fromList [("node_a", Data.Node "some name" Data.Record [] Nothing Map.empty)]
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
      let edgeA = Edge
            { _eType = Just "edge_a"
            , _eFrom = "node_a"
            , _eTo = "node_b"
            }
      let actual = toDataEdge baseEdgeTypes edgeA
      let expected = Data.Edge [Data.Dashed] "node_a" "node_b" Data.From
      actual `shouldBe` expected
