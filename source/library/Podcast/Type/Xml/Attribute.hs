module Podcast.Type.Xml.Attribute
  ( Attribute(..)
  )
where

import qualified Data.Text as Text

data Attribute = Attribute
  { name :: Text.Text
  , value :: Text.Text
  } deriving (Eq, Ord, Show)
