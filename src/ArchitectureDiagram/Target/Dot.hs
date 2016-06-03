{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ArchitectureDiagram.Target.Dot
  ( nodeStatement
  , NodeLeafest
  , toGraph
  , nodeLeafest
  , edgeStatement
  , listAllNodes
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

nodeStatement :: (NodeRef, Node) -> Statement
nodeStatement (ref, node) = 
  if Map.null (_nNodes node)
    then toNodeStatement ref node
    else toClusterStatement ref node

toNodeStatement :: NodeRef -> Node -> Statement
toNodeStatement ref node = NodeStatement (nodeId ref) (toNodeAttributes node)

toNodeAttributes :: Node -> [Attribute]
toNodeAttributes n = attributes ++ catMaybes mAttributes
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

nodeId :: NodeRef -> NodeId
nodeId ref = NodeId (StringId $ fromText (unNodeRef ref)) Nothing

label :: Text -> Attribute
label x = AttributeSetValue (NameId "label") (StringId $ fromText x)

shape :: Shape -> Attribute
shape x = AttributeSetValue (NameId "shape") (StringId $ shapeId x)

shapeId :: Shape -> String
shapeId Record = "record"
shapeId Box3d = "box3d"

nodeStyles :: [NodeStyle] -> Maybe Attribute
nodeStyles = style nodeStyleId

nodeStyleId :: NodeStyle -> String
nodeStyleId Rounded = "rounded"

width :: Float -> Attribute
width x = AttributeSetValue (NameId "width") (FloatId x)

toClusterStatement :: NodeRef -> Node -> Statement
toClusterStatement ref node = SubgraphStatement $ NewSubgraph (Just $ clusterId ref) statements
  where
    statements =
      [ AssignmentStatement (NameId "label") (StringId . fromText $ _nName node) ] ++
      (map nodeStatement . Map.toList $ _nNodes node)

clusterId :: NodeRef -> Id
clusterId ref = StringId . fromText $ "cluster_" `mappend` (unNodeRef ref)

edgeStatement :: (NodeLeafest, Edge) -> Statement
edgeStatement (nl@(NodeLeafest leafest), e) = EdgeStatement
  [ ENodeId NoEdge (nodeId fromEdge)
  , ENodeId DirectedEdge (nodeId toEdge)
  ]
  (toEdgeAttributes toEdgeLeafest fromEdgeLeafest e)
  where
    toEdge = fromMaybe (_eTo e) toEdgeLeafest
    toEdgeLeafest = Map.lookup (_eTo e) leafest
    fromEdge = fromMaybe (_eFrom e) fromEdgeLeafest
    fromEdgeLeafest = Map.lookup (_eFrom e) leafest

toEdgeAttributes :: Maybe NodeRef -> Maybe NodeRef -> Edge -> [Attribute]
toEdgeAttributes toEdge fromEdge e = attributes ++ catMaybes mAttributes
  where
    attributes :: [Attribute]
    attributes = []

    mAttributes :: [Maybe Attribute]
    mAttributes =
      [ rankAttribute (_eRank e)
      , toEdgeClusterAttribute
      , fromEdgeClusterAttribute
      , edgeStyles (_eStyles e)
      ]
    toEdgeClusterAttribute = case toEdge of
      Nothing -> Nothing
      Just _ -> Just $ case _eRank e of
        To -> AttributeSetValue (NameId "ltail") (clusterId $ _eTo e)
        From -> AttributeSetValue (NameId "lhead") (clusterId $ _eTo e)
    fromEdgeClusterAttribute = case fromEdge of
      Nothing -> Nothing
      Just _ -> Just $ case _eRank e of
        From -> AttributeSetValue (NameId "ltail") (clusterId $ _eFrom e)
        To -> AttributeSetValue (NameId "lhead") (clusterId $ _eFrom e)

rankAttribute :: EdgeRank -> Maybe Attribute
rankAttribute rank = case rank of
  To -> Just $ AttributeSetValue (NameId "dir") (StringId "back")
  From -> Nothing

edgeStyles :: [EdgeStyle] -> Maybe Attribute
edgeStyles = style edgeStyleId

edgeStyleId :: EdgeStyle -> String
edgeStyleId Dashed = "dashed"

style :: (a -> String) -> [a] -> Maybe Attribute
style f xs
  | null xs = Nothing
  | otherwise = Just $ AttributeSetValue (NameId "style") (StringId $ intercalate "," (map f xs))

toGraph :: Graph -> Dot.Graph
toGraph graph = Dot.Graph UnstrictGraph DirectedGraph (Just $ StringId (fromText $ _gName graph))
  ( 
    [ AssignmentStatement (NameId "ranksep") (IntegerId 1)
    , AssignmentStatement (NameId "rankdir") (NameId "TB")
    , AssignmentStatement (NameId "compound") (NameId "true")
    ] ++
    (map nodeStatement (Map.toList $ _gNodes graph)) ++
    (map (\e -> edgeStatement (leafest, e)) (_gEdges graph))
  )
  where
    leafest = nodeLeafest (_gNodes graph)

newtype NodeLeafest = NodeLeafest { unNodeLeafest :: Map NodeRef NodeRef }
  deriving (Show, Eq)

someChildNodeKey :: Node -> Maybe NodeRef
someChildNodeKey n = let
  mKeyNode = fmap fst (Map.minViewWithKey $ _nNodes n)
  mKey = fmap fst mKeyNode
  mNode = fmap snd mKeyNode
  mChildKey = someChildNodeKey =<< mNode
  in mChildKey <|> mKey

nodeLeafest :: Map NodeRef Node -> NodeLeafest
nodeLeafest nodes = NodeLeafest $ Map.fromList $ catMaybes (map nodeChild $ listAllNodes nodes)
  where
    nodeChild :: (NodeRef, Node) -> Maybe (NodeRef, NodeRef)
    nodeChild (ref, node) = (ref,) <$> someChildNodeKey node

listAllNodes :: Map NodeRef Node -> [(NodeRef, Node)]
listAllNodes nodes = Map.toList nodes ++ childCousins ++ grandCousins
  where
    grandCousins :: [(NodeRef, Node)]
    grandCousins = if null childCousins then [] else listAllNodes (Map.fromList childCousins)

    childCousins :: [(NodeRef, Node)]
    childCousins = concat . map (Map.toList . _nNodes . snd) . Map.toList $ nodes
