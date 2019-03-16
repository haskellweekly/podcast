module Podcast.Type.Description
  ( Description
  , fromString
  , toString
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text

newtype Description
  = Description Text.Text
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Description
fromString string = if all Char.isSpace string
  then Left ("invalid Description: " <> show string)
  else Right (Description (Text.pack string))

toString :: Description -> String
toString (Description text) = Text.unpack text
