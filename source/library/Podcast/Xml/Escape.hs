module Podcast.Xml.Escape
  ( escape
  )
where

escape :: String -> String
escape = concatMap escapeChar

escapeChar :: Char -> String
escapeChar char = case char of
  '\x22' -> "&quot;"
  '\x26' -> "&amp;"
  '\x27' -> "&apos;"
  '\x3c' -> "&lt;"
  '\x3e' -> "&gt;"
  _ -> [char]
