module Podcast.Type.Xml.Element
  ( Element(..)
  , Node
  )
where

import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Node as Node

data Element = Element
  { name :: String
  , attributes :: [Attribute.Attribute]
  , nodes :: [Node]
  } deriving (Eq, Ord, Show)

type Node = Node.Node Element
