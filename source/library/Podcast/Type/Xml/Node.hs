module Podcast.Type.Xml.Node
  ( Node(..)
  )
where

data Node element
  = Content String
  | Element element
  deriving (Eq, Ord, Show)
