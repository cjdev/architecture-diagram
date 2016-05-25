{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.NodeSpec (spec) where

import Test.Hspec

import Data.Text (Text)
import Language.Dot.Syntax

import ArchitectureDiagram.Data.Node
import ArchitectureDiagram.Data.ToStatement (toStatement)

spec :: Spec
spec = do
  describe "architecture diagram nodes to dot statements" $ do
    it "should convert the bare minimium node with a record shape" $ do
      let actual = toStatement $ Node "n" Record [] Nothing []
      let expected = NodeStatement
            (NodeId (StringId "n") Nothing)
            [ AttributeSetValue (NameId "shape") (StringId "record")
            ]
      actual `shouldBe` expected

    it "should convert the bare minimium node with a box3d shape" $ do
      let actual = toStatement $ Node "n" Box3d [] Nothing []
      let expected = NodeStatement
            (NodeId (StringId "n") Nothing)
            [ AttributeSetValue (NameId "shape") (StringId "box3d")
            ]
      actual `shouldBe` expected
    
    it "should convert the node with a rounded style" $ do
      let actual = toStatement $ Node "n" Record [Rounded] Nothing []
      let expected = NodeStatement
            (NodeId (StringId "n") Nothing)
            [ AttributeSetValue (NameId "shape") (StringId "record")
            , AttributeSetValue (NameId "style") (StringId "rounded")
            ]
      actual `shouldBe` expected
    
    it "should convert the node with a width" $ do
      let actual = toStatement $ Node "n" Record [] (Just 0) []
      let expected = NodeStatement
            (NodeId (StringId "n") Nothing)
            [ AttributeSetValue (NameId "shape") (StringId "record")
            , AttributeSetValue (NameId "width") (FloatId 0)
            ]
      actual `shouldBe` expected

    it "should convert the node to a subgraph" $ do
      let actual = toStatement $ Node "c" Record [] Nothing [Node "n" Record [] Nothing []]
      let expected = SubgraphStatement $ NewSubgraph (Just $ StringId "cluster_c")
            [ AssignmentStatement (NameId "label") (StringId "c")
            , NodeStatement
                (NodeId (StringId "n") Nothing)
                [ AttributeSetValue (NameId "shape") (StringId "record")
                ]
            ]
      actual `shouldBe` expected
