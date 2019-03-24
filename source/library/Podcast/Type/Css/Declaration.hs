module Podcast.Type.Css.Declaration
  ( Declaration(..)
  )
where

data Declaration = Declaration
  { property :: String
  , value :: String
  } deriving (Eq, Ord, Show)
