module Main ( main ) where
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

main :: IO ()
main = do
  let input = "input"
  let output = "output"
  Directory.createDirectoryIfMissing True output

  Directory.copyFile
    (FilePath.combine input "logo.png")
    (FilePath.combine output "haskell-weekly-podcast-logo.png")

  -- https://help.apple.com/itc/podcasts_connect/#/itcbaf351599
  writeFile (FilePath.combine output "haskell-weekly-podcast.rss") (concat
    [ "<?xml version='1.0' encoding='utf-8'?>"
    , "<rss version='2.0' xmlns:itunes='http://www.itunes.com/dtds/podcast-1.0.dtd' xmlns:content='http://purl.org/rss/1.0/modules/content/'>"
      , "<channel>"
        , "<title>Haskell Weekly</title>"
      , "</channel>"
    , "</rss>"
    ])

  writeFile (FilePath.combine output "index.html") (concat
    [ "<!doctype html>"
    , "<html>"
      , "<head>"
        , "<meta charset='utf-8'>"
        , "<title>Haskell Weekly Podcast</title>"
      , "<head>"
      , "</head>"
      , "<body>"
        , "<h1>Haskell Weekly Podcast</h1>"
        , "<p>"
          , "<a href='haskell-weekly-podcast.rss'>RSS feed</a>"
        , "</p>"
      , "</body>"
    , "</html>"
    ])
