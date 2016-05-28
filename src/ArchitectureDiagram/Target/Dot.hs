{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Target.Dot
  ( ToStatement(..)
  , toGraph
  ) where

import qualified Language.Dot.Syntax as Dot
import Data.Text (Text)
import Data.Text.Conversions (fromText)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Data.Node (Node(..), NodeStyle(..), Shape(..))
import ArchitectureDiagram.Data.Edge (Edge(..), EdgeStyle(..), EdgeRank(..))
import ArchitectureDiagram.Data.Graph (Graph(..))

class ToStatement a where
  toStatement :: a -> Statement

instance ToStatement Node where
  toStatement n = if null (_nChildren n) then toNodeStatement n else toClusterStatement n

toNodeStatement :: Node -> Statement
toNodeStatement n = NodeStatement (nodeId (_nRef n)) (toAttributes n)

toAttributes :: Node -> [Attribute]
toAttributes n = attributes ++ catMaybes mAttributes
  where
    attributes :: [Attribute]
    attributes =
      [ label (_nName n)
      , shape (_nShape n)
      ]
    mAttributes :: [Maybe Attribute]
    mAttributes =
      [ nodeStyles (_nStyles n)
      , width <$> _nWidth n
      ]

nodeId :: Text -> NodeId
nodeId x = NodeId (StringId $ fromText x) Nothing

label :: Text -> Attribute
label x = AttributeSetValue (NameId "label") (StringId $ fromText x)

shape :: Shape -> Attribute
shape x = AttributeSetValue (NameId "shape") (StringId $ shapeId x)

shapeId :: Shape -> String
shapeId Record = "record"
shapeId Box3d = "box3d"

nodeStyles :: [NodeStyle] -> Maybe Attribute
nodeStyles xs
  | null xs = Nothing
  | otherwise = Just $ AttributeSetValue (NameId "style") (StringId $ intercalate "," (map nodeStyleId xs))

nodeStyleId :: NodeStyle -> String
nodeStyleId Rounded = "rounded"

width :: Float -> Attribute
width x = AttributeSetValue (NameId "width") (FloatId x)

toClusterStatement :: Node -> Statement
toClusterStatement n = SubgraphStatement $ NewSubgraph (clusterId (_nName n)) statements
  where
    statements =
      [ AssignmentStatement (NameId "label") (StringId . fromText $ _nName n) ] ++
      (map toStatement $ _nChildren n)

clusterId :: Text -> Maybe Id
clusterId x = Just . StringId . fromText $ "cluster_" `mappend` x

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
