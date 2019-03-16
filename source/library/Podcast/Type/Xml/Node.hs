module Podcast.Type.Xml.Node
  ( Node(..)
  )
where

import qualified Data.Text as Text

data Node element
  = Content Text.Text
  | Element element
  deriving (Eq, Ord, Show)
