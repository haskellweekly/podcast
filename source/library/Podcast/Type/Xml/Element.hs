module Podcast.Type.Xml.Element
  ( Element(..)
  )
where

import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Node as Node

data Element = Element
  { name :: String
  , attributes :: [Attribute.Attribute]
  , nodes :: [Node.Node Element]
  } deriving (Eq, Ord, Show)
