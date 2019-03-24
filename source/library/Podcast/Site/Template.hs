module Podcast.Site.Template
  ( html
  )
where

import qualified Podcast.Html as Html
import qualified Podcast.Site.Logo as Logo
import qualified Podcast.Type.Route as Route
import qualified Podcast.Type.Url as Url

html :: Url.Url -> String -> Html.Node -> Html.Element
html root title content =
  Html.element "html" [] [head_ root title, body content]

head_ :: Url.Url -> String -> Html.Node
head_ root title = Html.node
  "head"
  []
  [ Html.node "meta" [("charset", "utf-8")] []
  , Html.node
    "meta"
    [ ("content", "initial-scale = 1, width = device-width")
    , ("name", "viewport")
    ]
    []
  , Html.node "title" [] [Html.text title]
  , Html.node
    "link"
    [ ("href", Url.toString (Url.combine root (Route.toUrl Route.Feed)))
    , ("rel", "alternate")
    , ("title", "Haskell Weekly podcast")
    , ("type", "application/rss+xml")
    ]
    []
  ]

body :: Html.Node -> Html.Node
body content = Html.node
  "body"
  []
  [ Html.node "h1" [] [Html.text "Haskell Weekly podcast"]
  , Html.node
    "div"
    [("style", "width: 100px; height: 100px; background: #5c3566;")]
    [Logo.svg]
  , content
  ]
