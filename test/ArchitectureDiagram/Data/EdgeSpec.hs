{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.EdgeSpec where

import Test.Hspec

import Data.Text (Text)
import Language.Dot.Syntax

import ArchitectureDiagram.Data.Edge
import ArchitectureDiagram.Data.ToStatement (toStatement)

spec :: Spec
spec = do
  describe "architecture diagram edges to dot statements" $ do
    it "should convert the bare minimium edge (a -> b)" $ do
      let actual = toStatement $ Edge [] "node_a" "node_b" From
      let expected = EdgeStatement
            [ ENodeId DirectedEdge (NodeId (StringId "node_a") Nothing )
            , ENodeId NoEdge (NodeId (StringId "node_b") Nothing )
            ]
            []
      actual `shouldBe` expected
