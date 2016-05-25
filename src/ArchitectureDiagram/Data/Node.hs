{-# LANGUAGE OverloadedStrings #-}
module ArchitectureDiagram.Data.Node
 ( Shape(..)
 , NodeStyle(..)
 , Node(..)
 ) where

import Data.Text (Text)
import Data.Text.Conversions (ToText(..), convertText, fromText)
import Data.List (intercalate)
import Language.Dot.Syntax

import ArchitectureDiagram.Data.ToStatement (ToStatement(..))

data Shape
  = Record
  | Box3d
  deriving (Show, Eq)

instance ToText Shape where
  toText Record = "record"
  toText Box3d = "box3d"

data NodeStyle
  = Rounded
  deriving (Show, Eq)

instance ToText NodeStyle where
  toText Rounded = "rounded"

data Node = Node
  { _nName :: Text
  , _nShape :: Shape
  , _nStyles :: [NodeStyle]
  , _nWidth :: Maybe Float
  , _nChildren :: [Node]
  } deriving (Show, Eq)

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
