module Podcast.Type.Description
  ( Description
  , fromString
  , toString
  )
where

import qualified Data.Text as Text

newtype Description
  = Description Text.Text
  deriving (Eq, Ord, Show)

fromString :: String -> Description
fromString = Description . Text.pack

toString :: Description -> String
toString (Description text) = Text.unpack text
