{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Target.DotSpec (spec) where

import Test.Hspec

import qualified Language.Dot.Syntax as Dot
import qualified Data.Map as Map
import Data.Text (Text)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Target.Dot (toStatement, toGraph, NodeLeafest, nodeLeafest)
import ArchitectureDiagram.Data.Node
import ArchitectureDiagram.Data.Edge
import ArchitectureDiagram.Data.Graph

prependStatements :: [Statement]
prependStatements =  
  [ AssignmentStatement (NameId "ranksep") (IntegerId 1)
  , AssignmentStatement (NameId "rankdir") (NameId "TB")
  , AssignmentStatement (NameId "compound") (NameId "true")
  ]

baseGraph :: Graph
baseGraph = Graph
  { _gName = "graph"
  , _gNodes = Map.empty
  , _gEdges = []
  , _gNodeTypes = Map.empty
  }

baseNode :: Node
baseNode = Node
  { _nName = "n"
  , _nShape = Record
  , _nStyles = []
  , _nWidth = Nothing
  , _nChildren = Map.empty
  }

baseNodeLeafest :: NodeLeafest
baseNodeLeafest = nodeLeafest Map.empty

baseEdge :: Edge
baseEdge = Edge [] "node_a" "node_b" From

baseNodeType :: NodeType
baseNodeType = NodeType
  { _ntStyles = []
  }

spec :: Spec
spec = do
  describe "architecture diagram nodes to dot statements" $ do
    it "should convert the bare minimium node with a record shape" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, baseNode)
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "record")
            ]
      actual `shouldBe` expected

    it "should convert the bare minimium node with a box3d shape" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, baseNode { _nShape = Box3d })
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "box3d")
            ]
      actual `shouldBe` expected
    
    it "should convert the node with a rounded style" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, baseNode { _nStyles = [Rounded] })
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "record")
            , AttributeSetValue (NameId "style") (StringId "rounded")
            ]
      actual `shouldBe` expected
    
    it "should convert the node with a width" $ do
      let actual = toStatement $ ("node_n" :: NodeRef, baseNode { _nWidth = Just 0 })
      let expected = NodeStatement
            (NodeId (StringId "node_n") Nothing)
            [ AttributeSetValue (NameId "label") (StringId "n")
            , AttributeSetValue (NameId "shape") (StringId "record")
            , AttributeSetValue (NameId "width") (FloatId 0)
            ]
      actual `shouldBe` expected

    it "should convert the node to a subgraph" $ do
      let actual = toStatement $ ("c" :: NodeRef, baseNode { _nName = "c", _nChildren = Map.fromList [("node_n", baseNode)] })
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
      let actual = toStatement (baseNodeLeafest, baseEdge)
      let expected = EdgeStatement
            [ ENodeId NoEdge (NodeId (StringId "node_a") Nothing )
            , ENodeId DirectedEdge (NodeId (StringId "node_b") Nothing )
            ]
            []
      actual `shouldBe` expected

    it "should convert an edge with edge rank 'to' (b <- a)" $ do
      let actual = toStatement (baseNodeLeafest, baseEdge { _eRank = To })
      let expected = EdgeStatement
            [ ENodeId NoEdge (NodeId (StringId "node_a") Nothing )
            , ENodeId DirectedEdge (NodeId (StringId "node_b") Nothing )
            ]
            [ AttributeSetValue (NameId "dir") (StringId "back") ]
      actual `shouldBe` expected

    it "should create an edge where the parent-node/cluster points to another node" $ do
      let leafest = nodeLeafest $ Map.fromList [(NodeRef "node_a", baseNode { _nChildren = Map.fromList [(NodeRef "node_c", baseNode)] })]
      let actual = toStatement (leafest, baseEdge)
      let expected = EdgeStatement
            [ ENodeId NoEdge (NodeId (StringId "node_c") Nothing )
            , ENodeId DirectedEdge (NodeId (StringId "node_b") Nothing )
            ]
            [ AttributeSetValue (NameId "ltail") (StringId "cluster_node_a") ]
      actual `shouldBe` expected

  describe "architecture diagram graph to dot graph" $ do
    it "should create an empty graph" $ do
      let actual = toGraph baseGraph { _gName = "empty" }
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "empty") prependStatements
      actual `shouldBe` expected

    it "should create a graph with a node" $ do
      let nodeA = ("node_a", baseNode { _nName = "a" })
      let nodes = Map.fromList [nodeA]
      let actual = toGraph baseGraph { _gNodes = nodes }
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA ])
      actual `shouldBe` expected

    it "should create a graph with nodes" $ do
      let nodeA = ("node_a" :: NodeRef, baseNode { _nName = "a" })
      let nodeB = ("node_b" :: NodeRef, baseNode { _nName = "b" })
      let nodes = Map.fromList [nodeA, nodeB]
      let actual = toGraph baseGraph { _gNodes = nodes }
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA, toStatement nodeB ])
      actual `shouldBe` expected

    it "should create a graph with a node type" $ do
      let nodeTypeA = ("type_a", baseNodeType)
      let nodeTypes = Map.fromList [nodeTypeA]
      let actual = toGraph baseGraph { _gNodeTypes = nodeTypes }
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [])
      actual `shouldBe` expected

    it "should create a graph with nodes and an edge" $ do
      let nodeA = ("node_a" :: NodeRef, baseNode { _nName = "a" })
      let nodeB = ("node_b" :: NodeRef, baseNode { _nName = "b" })
      let edgeA = baseEdge
      let actual = toGraph baseGraph { _gNodes = Map.fromList [nodeA, nodeB], _gEdges = [edgeA] }
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA, toStatement nodeB, toStatement (baseNodeLeafest, edgeA) ])
      actual `shouldBe` expected
