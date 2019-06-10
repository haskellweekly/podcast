module Podcast.Type.Transcript
  ( Transcript
  , fromString
  , toString
  )
where

newtype Transcript
  = Transcript String
  deriving (Eq, Ord, Show)

fromString :: String -> Transcript
fromString = Transcript

toString :: Transcript -> String
toString (Transcript string) = string
