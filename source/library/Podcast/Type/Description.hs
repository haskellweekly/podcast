module Podcast.Type.Description
  ( Description
  , fromString
  , toString
  )
where

newtype Description
  = Description String
  deriving (Eq, Ord, Show)

fromString :: String -> Description
fromString = Description

toString :: Description -> String
toString (Description string) = string
