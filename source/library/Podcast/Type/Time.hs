module Podcast.Type.Time
  ( Time
  , fromIso8601
  , toDateString
  , toRfc822
  )
where

import qualified Data.Time as Time

newtype Time
  = Time Time.UTCTime
  deriving (Eq, Ord, Show)

fromIso8601 :: String -> Either String Time
fromIso8601 string = case Time.parseTimeM False locale iso8601 string of
  Nothing -> Left ("invalid Time: " ++ show string)
  Just utcTime -> Right (Time utcTime)

toDateString :: Time -> String
toDateString (Time utcTime) = Time.formatTime locale "%Y-%m-%d" utcTime

toRfc822 :: Time -> String
toRfc822 (Time utcTime) = Time.formatTime locale rfc822 utcTime

locale :: Time.TimeLocale
locale = Time.defaultTimeLocale

iso8601 :: String
iso8601 = Time.iso8601DateFormat (Just "%T")

rfc822 :: String
rfc822 = "%a, %d %b %Y %H:%M:%S GMT"
