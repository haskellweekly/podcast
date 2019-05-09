module Podcast.Type.Date
  ( Date
  , fromGregorian
  , toDateString
  , toRfc822
  )
where

import qualified Data.Time as Time

newtype Date
  = Date Time.Day
  deriving (Eq, Ord, Show)

fromGregorian :: Integer -> Int -> Int -> Either String Date
fromGregorian y m d = case Time.fromGregorianValid y m d of
  Nothing -> Left ("invalid Date: " <> show (y, m, d))
  Just day -> Right (Date day)

toDateString :: Date -> String
toDateString (Date day) = Time.formatTime locale "%Y-%m-%d" day

toRfc822 :: Date -> String
toRfc822 (Date day) = Time.formatTime locale "%a, %d %b %Y 12:00:00 GMT" day

locale :: Time.TimeLocale
locale = Time.defaultTimeLocale
