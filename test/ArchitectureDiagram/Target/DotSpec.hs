{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Target.DotSpec (spec) where

import Test.Hspec

import qualified Language.Dot.Syntax as Dot
import qualified Data.Map as Map
import Data.Text (Text)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Target.Dot (toStatement, toGraph)
import ArchitectureDiagram.Data.Node
import ArchitectureDiagram.Data.Edge
import ArchitectureDiagram.Data.Graph

prependStatements :: [Statement]
prependStatements =  
  [ AssignmentStatement (NameId "ranksep") (IntegerId 1)
  , AssignmentStatement (NameId "rankdir") (NameId "TB")
  , AssignmentStatement (NameId "compound") (NameId "true")
  ] 

spec :: Spec
spec = do
  describe "architecture diagram nodes to dot statements" $ do
    it "should convert the bare minimium node with a record shape" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, Node "n" Record [] Nothing Map.empty)
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "record")
            ]
      actual `shouldBe` expected

    it "should convert the bare minimium node with a box3d shape" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, Node "n" Box3d [] Nothing Map.empty)
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "box3d")
            ]
      actual `shouldBe` expected
    
    it "should convert the node with a rounded style" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, Node "n" Record [Rounded] Nothing Map.empty)
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "record")
            , AttributeSetValue (NameId "style") (StringId "rounded")
            ]
      actual `shouldBe` expected
    
    it "should convert the node with a width" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, Node "n" Record [] (Just 0) Map.empty)
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "record")
            , AttributeSetValue (NameId "width") (FloatId 0)
            ]
      actual `shouldBe` expected

    it "should convert the node to a subgraph" $ do
      let actual = toStatement $ ("c" :: NodeRef, Node "c" Record [] Nothing (Map.fromList [("node_n", Node "n" Record [] Nothing Map.empty)]))
      let expected = SubgraphStatement $ NewSubgraph (Just $ StringId "cluster_c")
            [ AssignmentStatement (NameId "label") (StringId "c")
            , NodeStatement
                (NodeId (StringId "node_n") Nothing)
                [ AttributeSetValue (NameId "label") (StringId "n")
                , AttributeSetValue (NameId "shape") (StringId "record")
                ]
            ]
      actual `shouldBe` expected

  describe "architecture diagram edges to dot statements" $ do
    it "should convert an edge with edge rank 'from' (a -> b)" $ do
      let actual = toStatement $ Edge [] "node_a" "node_b" From
      let expected = EdgeStatement
            [ ENodeId NoEdge (NodeId (StringId "node_a") Nothing )
            , ENodeId DirectedEdge (NodeId (StringId "node_b") Nothing )
            ]
            []
      actual `shouldBe` expected
    it "should convert an edge with edge rank 'to' (b <- a)" $ do
      let actual = toStatement $ Edge [] "node_a" "node_b" To
      let expected = EdgeStatement
            [ ENodeId NoEdge (NodeId (StringId "node_a") Nothing )
            , ENodeId DirectedEdge (NodeId (StringId "node_b") Nothing )
            ]
            [ AttributeSetValue (NameId "dir") (StringId "back") ]
      actual `shouldBe` expected

  describe "architecture diagram graph to dot graph" $ do
    it "should create an empty graph" $ do
      let actual = toGraph $ Graph "empty" Map.empty []
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "empty") prependStatements
      actual `shouldBe` expected

    it "should create a graph with a node" $ do
      let nodeA = ("node_a", Node "a" Record [] Nothing Map.empty)
      let nodes = Map.fromList [nodeA]
      let actual = toGraph $ Graph "graph" nodes []
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA ])
      actual `shouldBe` expected

    it "should create a graph with nodes" $ do
      let nodeA = ("node_a" :: NodeRef, Node "a" Record [] Nothing Map.empty)
      let nodeB = ("node_b" :: NodeRef, Node "b" Record [] Nothing Map.empty)
      let nodes = Map.fromList [nodeA, nodeB]
      let actual = toGraph $ Graph "graph" nodes []
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA, toStatement nodeB ])
      actual `shouldBe` expected

    it "should create a graph with nodes and an edge" $ do
      let nodeA = ("node_a", Node "a" Record [] Nothing Map.empty)
      let nodeB = ("node_b", Node "b" Record [] Nothing Map.empty)
      let edgeA = Edge [] "node_a" "node_b" From
      let actual = toGraph $ Graph "graph" (Map.fromList [nodeA, nodeB]) [edgeA]
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA, toStatement nodeB, toStatement edgeA ])
      actual `shouldBe` expected
