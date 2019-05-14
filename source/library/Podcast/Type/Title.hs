module Podcast.Type.Title
  ( Title
  , fromString
  , toString
  )
where

import qualified Data.Char as Char

newtype Title
  = Title String
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Title
fromString string = if all Char.isSpace string
  then Left ("invalid Title: " ++ show string)
  else Right (Title string)

toString :: Title -> String
toString (Title string) = string
