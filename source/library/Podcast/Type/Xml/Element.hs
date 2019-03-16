module Podcast.Type.Xml.Element
  ( Element(..)
  )
where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Podcast.Type.Xml.Attribute as Attribute

data Element node = Element
  { name :: Text.Text
  , attributes :: Vector.Vector Attribute.Attribute
  , nodes :: Vector.Vector node
  } deriving (Eq, Ord, Show)
