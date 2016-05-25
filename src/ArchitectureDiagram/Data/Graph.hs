{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Graph
  ( Graph(..)
  , toGraph
  ) where

import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Data.Text.Conversions (fromText)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Data.Node (Node)
import ArchitectureDiagram.Data.ToStatement (toStatement)

data Graph = Graph
  { _gName :: Text
  , _gNodes :: [Node]
  } deriving (Show, Eq)

toGraph :: Graph -> Dot.Graph
toGraph graph = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId (fromText $ _gName graph))
  ( 
    [ AssignmentStatement (NameId "ranksep") (IntegerId 1)
    , AssignmentStatement (NameId "rankdir") (NameId "TB")
    , AssignmentStatement (NameId "compound") (NameId "true")
    ] ++
    (map toStatement (_gNodes graph))
  )


