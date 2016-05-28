{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Target.Dot
  ( ToStatement(..)
  , toGraph
  ) where

import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Data.Text.Conversions (ToText(..), convertText, fromText)
import Data.List (intercalate)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Data.Node (Node(..), NodeStyle(..), Shape(..))
import ArchitectureDiagram.Data.Edge (Edge(..), EdgeStyle(..), EdgeRank(..))
import ArchitectureDiagram.Data.Graph (Graph(..))

class ToStatement a where
  toStatement :: a -> Statement

instance ToStatement Node where
  toStatement n = if null (_nChildren n) then toNodeStatement n else toClusterStatement n

toNodeStatement :: Node -> Statement
toNodeStatement n = NodeStatement
  (NodeId (StringId $ fromText (_nName n)) Nothing)
  (
    [ AttributeSetValue (NameId "shape") (StringId $ convertText (_nShape n)) ] ++
    (if null (_nStyles n) then [] else [ AttributeSetValue (NameId "style") (StringId $ intercalate "," (map convertText (_nStyles n))) ]) ++
    (maybe [] (\width -> [ AttributeSetValue (NameId "width") (FloatId width) ]) (_nWidth n))
  )

toClusterStatement :: Node -> Statement
toClusterStatement n = SubgraphStatement $ NewSubgraph
  (Just . StringId . fromText $ "cluster_" `mappend` _nName n)
  ( 
    [ AssignmentStatement (NameId "label") (StringId . fromText $ _nName n) ] ++
    (map toStatement $ _nChildren n)
  )

instance ToStatement Edge where
  toStatement e = EdgeStatement
    [ ENodeId NoEdge (NodeId (StringId $ fromText (_eFrom e)) Nothing)
    , ENodeId DirectedEdge (NodeId (StringId $ fromText (_eTo e)) Nothing)
    ]
    rankAttribute
    where
      rankAttribute = case _eRank e of
        To -> [ AttributeSetValue (NameId "dir") (StringId "back") ]
        From -> []

toGraph :: Graph -> Dot.Graph
toGraph graph = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId (fromText $ _gName graph))
  ( 
    [ AssignmentStatement (NameId "ranksep") (IntegerId 1)
    , AssignmentStatement (NameId "rankdir") (NameId "TB")
    , AssignmentStatement (NameId "compound") (NameId "true")
    ] ++
    (map toStatement (_gNodes graph)) ++
    (map toStatement (_gEdges graph))
  )
