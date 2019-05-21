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
  Html.element "html" [] [head_ root title, body root content]

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
  , Html.node
    "link"
    [ ("href", Url.toString (Url.combine root (Route.toUrl Route.Bootstrap)))
    , ("rel", "stylesheet")
    ]
    []
  ]

body :: Url.Url -> Html.Node -> Html.Node
body root content = Html.node
  "body"
  []
  [ Html.node
      "div"
      [("class", "container")]
      [ Html.node
        "nav"
        [ ("class", "mb-3 mt-3 navbar navbar-dark rounded text-light")
        , ("style", "background-color: #5e5086;")
        ]
        [ Html.node
          "a"
          [ ("class", "navbar-brand")
          , ("href", Url.toString (Url.combine root (Route.toUrl Route.Index)))
          ]
          [Html.text "Haskell Weekly podcast"]
        , Html.node
          "span"
          [("class", "d-inline-block"), ("style", "width: 2em; height: 2em;")]
          [Logo.svg]
        ]
      , content
      , Html.node
        "div"
        [("class", "bg-light card mb-3 mt-3")]
        [ Html.node
            "div"
            [("class", "card-body")]
            [ Html.text "Content on this site is licensed under a "
            , Html.node
              "a"
              [("href", "https://creativecommons.org/licenses/by/4.0/")]
              [Html.text "Creative Commons Attribution 4.0 International"]
            , Html.text " license. The source code for this site is available "
            , Html.node
              "a"
              [("href", "https://github.com/haskellweekly/podcast")]
              [Html.text "on GitHub"]
            , Html.text ". You can subscribe to the "
            , Html.node
              "a"
              [("href", Url.toString (Url.combine root (Route.toUrl Route.Feed)))]
              [Html.text "Atom feed"]
            , Html.text "."
            ]
        ]
      ]
  ]
