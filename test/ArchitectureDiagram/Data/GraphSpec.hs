{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.GraphSpec (spec) where

import Test.Hspec

import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Data.ToStatement
import ArchitectureDiagram.Data.Node
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
      let actual = toDotGraph $ Graph "empty" []
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "empty") prependStatements
      actual `shouldBe` expected
    it "should create a graph with a node" $ do
      let stubNode = Node "nodey" Record [] Nothing []
      let actual = toDotGraph $ Graph "noder" [stubNode]
      let expected = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId "noder") 
            (prependStatements ++ [ toStatement stubNode ] )
      actual `shouldBe` expected
