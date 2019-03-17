module Podcast.Type.Xml.Attribute
  ( Attribute(..)
  )
where

data Attribute = Attribute
  { name :: String
  , value :: String
  } deriving (Eq, Ord, Show)
