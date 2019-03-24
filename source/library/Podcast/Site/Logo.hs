module Podcast.Site.Logo
  ( svg
  )
where

import qualified Podcast.Xml as Xml

svg :: Xml.Node
svg = Xml.node
  "svg"
  [("xmlns", "http://www.w3.org/2000/svg"), ("viewBox", "0 0 100 100")]
  [ Xml.node
    "path"
    [ ("fill", "#fff")
    , ("opacity", "0.8")
    , ( "d"
      , unwords
        [ "M"
        , "80.8"
        , "49.9"
        , "l"
        , "-19"
        , "28.4"
        , "H"
        , "76"
        , "l"
        , "19"
        , "-28.4"
        , "z"
        , "m"
        , "-61.6"
        , "0"
        , "l"
        , "19"
        , "28.4"
        , "H"
        , "24"
        , "L"
        , "5"
        , "49.9"
        , "z"
        ]
      )
    ]
    []
  , Xml.node
    "path"
    [ ("fill", "#fff")
    , ( "d"
      , unwords
        [ "M"
        , "24"
        , "78.3"
        , "l"
        , "19"
        , "-28.4"
        , "-19"
        , "-28.4"
        , "h"
        , "14.2"
        , "l"
        , "38"
        , "56.8"
        , "H"
        , "61.8"
        , "L"
        , "50"
        , "60.5"
        , "l"
        , "-11.8"
        , "18"
        , "H"
        , "24"
        , "z"
        , "m"
        , "0"
        , "0"
        ]
      )
    ]
    []
  ]
