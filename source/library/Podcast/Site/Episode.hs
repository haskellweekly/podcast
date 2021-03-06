module Podcast.Site.Episode
  ( html
  )
where

import qualified Podcast.Html as Html
import qualified Podcast.Site.Template as Template
import qualified Podcast.Type.Articles as Articles
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Route as Route
import qualified Podcast.Type.Title as Title
import qualified Podcast.Type.Transcript as Transcript
import qualified Podcast.Type.Url as Url

html :: Url.Url -> Episode.Episode -> Html.Element
html root episode = Template.html
  root
  ("Haskell Weekly podcast episode "
  ++ Number.toString (Episode.number episode)
  )
  [ Html.node
    "meta"
    [ ("property", "og:title")
    , ("content", Title.toString (Episode.title episode))
    ]
    []
  , Html.node
    "meta"
    [ ("property", "og:image")
    , ("content", Url.toString (Url.combine root (Route.toUrl Route.Logo)))
    ]
    []
  , Html.node
    "meta"
    [ ("property", "og:url")
    , ( "content"
      , Url.toString
        (Url.combine
          root
          (Route.toUrl (Route.Episode (Episode.number episode)))
        )
      )
    ]
    []
  , Html.node
    "meta"
    [ ("property", "og:audio")
    , ("content", Media.toString (Episode.media episode))
    ]
    []
  , Html.node
    "meta"
    [ ("property", "og:description")
    , ("content", Description.toString (Episode.description episode))
    ]
    []
  , Html.node
    "meta"
    [("property", "og:site_name"), ("content", "Haskell Weekly podcast")]
    []
  , Html.node "meta" [("name", "twitter:card"), ("content", "summary")] []
  , Html.node
    "meta"
    [("name", "twitter:site"), ("content", "@haskellweekly")]
    []
  ]
  (Html.node
    "div"
    []
    [ Html.node
      "h2"
      []
      [ Html.text "#"
      , Html.text (Number.toString (Episode.number episode))
      , Html.text ": "
      , Html.text (Title.toString (Episode.title episode))
      ]
    , Html.node
      "h3"
      [("class", "text-muted")]
      [Html.text (Date.toDateString (Episode.date episode))]
    , Html.node
      "div"
      [("class", "row")]
      [ Html.node
        "div"
        [("class", "col-md-4 mb-3 mt-3")]
        [ Html.node
            "audio"
            [ ("class", "d-block w-100")
            , ("controls", "controls")
            , ("preload", "metadata")
            , ("src", Media.toString (Episode.media episode))
            ]
            [Html.text ""]
        ]
      , Html.node
        "div"
        [("class", "col")]
        [ Html.node
            "div"
            [("class", "card")]
            [ Html.node
                "div"
                [("class", "card-body")]
                [ Html.node
                  "p"
                  []
                  [ Html.text
                      (Description.toString (Episode.description episode))
                  ]
                , Html.node
                  "ul"
                  []
                  (fmap
                    (\article -> Html.node
                      "li"
                      []
                      [ Html.node
                          "a"
                          [ ("href", article)
                          , ("style", "word-break: break-all;")
                          ]
                          [Html.text article]
                      ]
                    )
                    (Articles.toStrings (Episode.articles episode))
                  )
                ]
            ]
        ]
      ]
    , Html.node
      "div"
      [("class", "row")]
      [ Html.node
          "div"
          [("class", "col")]
          [ Html.node
              "div"
              [("class", "card mt-3")]
              [ Html.node
                "div"
                [("class", "card-header")]
                [Html.text "Transcript"]
              , Html.node
                "div"
                [("class", "card-body"), ("style", "white-space: pre-line;")]
                [ case Episode.transcript episode of
                    Nothing ->
                      Html.text "No transcript is available for this episode."
                    Just transcript ->
                      Html.text (Transcript.toString transcript)
                ]
              ]
          ]
      ]
    ]
  )
