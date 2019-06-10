module Podcast.Site.Episode
  ( html
  )
where

import qualified Podcast.Html as Html
import qualified Podcast.Site.Template as Template
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Title as Title
import qualified Podcast.Type.Url as Url

html :: Url.Url -> Episode.Episode -> Html.Element
html root episode = Template.html
  root
  ("Haskell Weekly podcast episode "
  ++ Number.toString (Episode.number episode)
  )
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
            , ("src", Media.toString (Episode.url episode))
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
                [("class", "card-body"), ("style", "white-space: pre-wrap;")]
                [ Html.text
                    (Description.toString (Episode.description episode))
                ]
            ]
        ]
      ]
    ]
  )
