module Podcast.Site.Template
  ( html
  )
where

import qualified Podcast.Css as Css
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
  , Html.node "style" [] [Html.text (Css.render styles)]
  ]

styles :: [Css.Rule]
styles =
  [ Css.rule "body" [("margin", "0 auto"), ("max-width", "40em")]
  , Css.rule
    ".logo"
    [("background", "#5c3566"), ("height", "100px"), ("width", "100px")]
  , Css.rule ".badges" [("text-align", "center")]
  ]

body :: Html.Node -> Html.Node
body content = Html.node
  "body"
  []
  [ Html.node "h1" [] [Html.text "Haskell Weekly podcast"]
  , Html.node "div" [("class", "logo")] [Logo.svg]
  , content
  ]
