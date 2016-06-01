{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ArchitectureDiagram.Target.Dot
  ( ToStatement(..)
  , NodeLeafest
  , toGraph
  , nodeLeafest
  ) where

import qualified Language.Dot.Syntax as Dot
import qualified Data.Map as Map
import Control.Applicative
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Conversions (fromText)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Language.Dot.Syntax hiding (Graph)

import ArchitectureDiagram.Data.Node (Node(..), NodeRef(..), NodeStyle(..), Shape(..))
import ArchitectureDiagram.Data.Edge (Edge(..), EdgeStyle(..), EdgeRank(..))
import ArchitectureDiagram.Data.Graph (Graph(..))

class ToStatement a where
  toStatement :: a -> Statement

instance ToStatement (NodeRef, Node) where
  toStatement (ref, node) =
    if Map.null (_nChildren node)
      then toNodeStatement ref node
      else toClusterStatement ref node

toNodeStatement :: NodeRef -> Node -> Statement
toNodeStatement ref node = NodeStatement (nodeId (unNodeRef ref)) (toAttributes node)

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

toClusterStatement :: NodeRef -> Node -> Statement
toClusterStatement ref node = SubgraphStatement $ NewSubgraph (clusterId ref) statements
  where
    statements =
      [ AssignmentStatement (NameId "label") (StringId . fromText $ _nName node) ] ++
      (map toStatement . Map.toList $ _nChildren node)

clusterId :: NodeRef -> Maybe Id
clusterId ref = Just . StringId . fromText $ "cluster_" `mappend` (unNodeRef ref)

instance ToStatement (NodeLeafest, Edge) where
  toStatement (NodeLeafest leafest, e) = EdgeStatement
    [ ENodeId NoEdge (NodeId (StringId $ fromText fromEdge) Nothing)
    , ENodeId DirectedEdge (NodeId (StringId $ fromText toEdge) Nothing)
    ]
    (rankAttribute ++ toEdgeClusterAttribute ++ fromEdgeClusterAttribute)
    where
      rankAttribute = case _eRank e of
        To -> [ AttributeSetValue (NameId "dir") (StringId "back") ]
        From -> []

      toEdge = fromMaybe (_eTo e) toEdgeLeafest
      toEdgeLeafest = unNodeRef <$> Map.lookup (NodeRef $ _eTo  e) leafest

      toEdgeClusterAttribute = case toEdgeLeafest of
        Nothing -> []
        Just _ -> let
          cluster = fromText $ "cluster_" `mappend` _eTo e
          in case _eRank e of
            To -> [ AttributeSetValue (NameId "ltail") (StringId cluster) ]
            From -> [ AttributeSetValue (NameId "lhead") (StringId cluster) ]

      fromEdge = fromMaybe (_eFrom e) fromEdgeLeafest
      fromEdgeLeafest = unNodeRef <$> Map.lookup (NodeRef $ _eFrom e) leafest

      fromEdgeClusterAttribute = case fromEdgeLeafest of
        Nothing -> []
        Just _ -> let
          cluster = fromText $ "cluster_" `mappend` _eFrom e
          in case _eRank e of
            From -> [ AttributeSetValue (NameId "ltail") (StringId cluster) ]
            To -> [ AttributeSetValue (NameId "lhead") (StringId cluster) ]


toGraph :: Graph -> Dot.Graph
toGraph graph = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId (fromText $ _gName graph))
  ( 
    [ AssignmentStatement (NameId "ranksep") (IntegerId 1)
    , AssignmentStatement (NameId "rankdir") (NameId "TB")
    , AssignmentStatement (NameId "compound") (NameId "true")
    ] ++
    (map toStatement (Map.toList $ _gNodes graph)) ++
    (map (\e -> toStatement (leafest, e)) (_gEdges graph))
  )
  where
    leafest = nodeLeafest (_gNodes graph)

newtype NodeLeafest = NodeLeafest { unNodeLeafest :: Map NodeRef NodeRef }
  deriving (Show, Eq)

someChildNodeKey :: Node -> Maybe NodeRef
someChildNodeKey n = let
  mKeyNode = fmap fst (Map.minViewWithKey $ _nChildren n)
  mKey = fmap fst mKeyNode
  mNode = fmap snd mKeyNode
  mChildKey = someChildNodeKey =<< mNode
  in mChildKey <|> mKey

nodeLeafest :: Map NodeRef Node -> NodeLeafest
nodeLeafest nodes = NodeLeafest $ Map.fromList $ catMaybes (map nodeChild $ Map.toList nodes)
  where
    nodeChild :: (NodeRef, Node) -> Maybe (NodeRef, NodeRef)
    nodeChild (ref, node) = (ref,) <$> someChildNodeKey node
