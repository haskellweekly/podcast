module Podcast.Type.Description
  ( Description
  , fromString
  , toString
  )
where

import qualified Data.Char as Char

newtype Description
  = Description String
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Description
fromString string = if all Char.isSpace string
  then Left ("invalid Description: " <> show string)
  else Right (Description string)

toString :: Description -> String
toString (Description string) = string
