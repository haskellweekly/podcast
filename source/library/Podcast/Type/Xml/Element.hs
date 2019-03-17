module Podcast.Type.Xml.Element
  ( Element(..)
  )
where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Node as Node

data Element = Element
  { name :: Text.Text
  , attributes :: Vector.Vector Attribute.Attribute
  , nodes :: Vector.Vector (Node.Node Element)
  } deriving (Eq, Ord, Show)
