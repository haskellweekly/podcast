module Main ( main ) where
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

main :: IO ()
main = do
  let directory = "site"
  Directory.createDirectoryIfMissing True directory
  writeFile (FilePath.combine directory "index.html") (concat
    [ "<!doctype html>"
    , "<html>"
      , "<head>"
        , "<meta charset='utf-8'>"
        , "<title>Haskell Weekly Podcast</title>"
      , "<head>"
      , "</head>"
      , "<body>"
        , "<h1>Haskell Weekly Podcast</h1>"
        , "<p>It's coming soon!</p>"
      , "</body>"
    , "</html>"
    ])
