module Podcast.Site.Episode
  ( html
  )
where

import qualified Podcast.Html as Html
import qualified Podcast.Site.Template as Template
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Time as Time
import qualified Podcast.Type.Url as Url

html :: Url.Url -> Episode.Episode -> Html.Element
html root episode = Template.html
  root
  ("Haskell Weekly -> Podcast -> Episode "
  ++ Number.toString (Episode.number episode)
  )
  (Html.node
    "div"
    []
    [ Html.node
      "h2"
      []
      [ Html.text "Episode "
      , Html.text (Number.toString (Episode.number episode))
      ]
    , Html.node
      "p"
      []
      [Html.text (Description.toString (Episode.description episode))]
    , Html.node
      "audio"
      [("controls", "controls"), ("src", Url.toString (Episode.url episode))]
      [Html.text ""]
    , Html.node
      "p"
      []
      [ Html.text "Published on "
      , Html.text (Time.toDateString (Episode.time episode))
      , Html.text "."
      ]
    ]
  )
