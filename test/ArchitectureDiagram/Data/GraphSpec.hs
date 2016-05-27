{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.GraphSpec (spec) where

import Test.Hspec

import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Data.ToStatement
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
  describe "architecture diagram graph to dot graph" $ do
    it "should create an empty graph" $ do
      let actual = toDotGraph $ Graph "empty" [] []
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "empty") prependStatements
      actual `shouldBe` expected

    it "should create a graph with a node" $ do
      let nodeA = Node "node_a" Record [] Nothing []
      let actual = toDotGraph $ Graph "graph" [nodeA] []
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA ])
      actual `shouldBe` expected

    it "should create a graph with nodes" $ do
      let nodeA= Node "node_a" Record [] Nothing []
      let nodeB = Node "node_b" Record [] Nothing []
      let actual = toDotGraph $ Graph "graph" [nodeA, nodeB] []
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA, toStatement nodeB ])
      actual `shouldBe` expected

    it "should create a graph with nodes and an edge" $ do
      let nodeA= Node "node_a" Record [] Nothing []
      let nodeB = Node "node_b" Record [] Nothing []
      let edgeA = Edge [] "node_a" "node_b" From
      let actual = toDotGraph $ Graph "graph" [nodeA, nodeB] [edgeA]
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "graph")
            (prependStatements ++ [ toStatement nodeA, toStatement nodeB, toStatement edgeA ])
      actual `shouldBe` expected
