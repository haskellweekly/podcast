module Podcast.Type.Time
  ( Time
  , fromString
  , toString
  )
where

import qualified Data.Time as Time

newtype Time
  = Time Time.UTCTime
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Time
fromString string = case Time.parseTimeM False locale iso8601 string of
  Nothing -> Left ("invalid Time: " ++ show string)
  Just utcTime -> Right (Time utcTime)

toString :: Time -> String
toString (Time utcTime) = dropEnd 1 (Time.formatTime locale rfc822 utcTime)

locale :: Time.TimeLocale
locale = Time.defaultTimeLocale

iso8601 :: String
iso8601 = Time.iso8601DateFormat (Just "%T")

rfc822 :: String
rfc822 = Time.rfc822DateFormat

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = zipWith const xs (drop n xs)
