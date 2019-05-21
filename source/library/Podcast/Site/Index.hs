module Podcast.Site.Index
  ( html
  )
where

import qualified Podcast.Html as Html
import qualified Podcast.Site.Template as Template
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Route as Route
import qualified Podcast.Type.Title as Title
import qualified Podcast.Type.Url as Url

html :: Url.Url -> [Episode.Episode] -> Html.Element
html root episodes =
  Template.html root "Haskell Weekly podcast" (body root episodes)

body :: Url.Url -> [Episode.Episode] -> Html.Node
body root episodes = Html.node
  "div"
  []
  [ Html.node
    "div"
    [("class", "jumbotron")]
    [ Html.node "h1" [] [Html.text "Haskell Weekly podcast"]
    , Html.node
      "p"
      [("class", "lead")]
      [ Html.text
          "Haskell Weekly is a podcast covering the Haskell programming \
          \langauge. Listen to professional software developers discuss using \
          \functional programming to solve real-world business problems. Each \
          \episode uses a conversational two-host format and runs for about \
          \15 minutes."
      ]
    , Html.node
      "div"
      [("class", "text-center")]
      [ Html.node "span" [("class", "d-inline-block p-1")] [appleBadge root]
      , Html.node "span" [("class", "d-inline-block p-1")] [googleBadge root]
      ]
    ]
  , Html.node "div" [("class", "row")] (map (item root) episodes)
  ]

item :: Url.Url -> Episode.Episode -> Html.Node
item root episode = Html.node
  "div"
  [("class", "col-md-4")]
  [ Html.node
      "div"
      [("class", "card mb-3")]
      [ Html.node
          "div"
          [("class", "card-body")]
          [ Html.node
            "h5"
            [("class", "card-title")]
            [ Html.text "#"
            , Html.text (Number.toString (Episode.number episode))
            , Html.text ": "
            , Html.text (Title.toString (Episode.title episode))
            ]
          , Html.node
            "h6"
            [("class", "card-subtitle mb-3 text-muted")]
            [Html.text (Date.toDateString (Episode.date episode))]
          , Html.node
            "p"
            [("class", "card-text")]
            [ Html.text
                (concat
                  (take
                    1
                    (lines (Description.toString (Episode.description episode))
                    )
                  )
                )
            ]
          , Html.node
            "a"
            [ ("class", "card-link")
            , ( "href"
              , Url.toString
                (Url.combine
                  root
                  (Route.toUrl (Route.Episode (Episode.number episode)))
                )
              )
            ]
            [Html.text "Listen now"]
          ]
      ]
  ]

appleBadge :: Url.Url -> Html.Node
appleBadge root = Html.node
  "a"
  [ ( "href"
    , "https://itunes.apple.com/us/podcast/haskell-weekly/id1456545040?mt=2&app=podcast"
    )
  ]
  [ Html.node
      "img"
      [ ("alt", "Listen on Apple Podcasts")
      , ("src", Url.toString (Url.combine root (Route.toUrl Route.AppleBadge)))
      , ("width", "200")
      , ("height", "49")
      ]
      []
  ]

googleBadge :: Url.Url -> Html.Node
googleBadge root = Html.node
  "a"
  [ ( "href"
    , "https://playmusic.app.goo.gl/?ibi=com.google.PlayMusic&isi=691797987&ius=googleplaymusic&apn=com.google.android.music&link=https://play.google.com/music/m/Irjo4hxyfeiid3zhasycmgs3o2q?t%3DHaskell_Weekly%26pcampaignid%3DMKT-na-all-co-pr-mu-pod-16"
    )
  ]
  [ Html.node
      "img"
      [ ("alt", "Listen on Google Podcasts")
      , ( "src"
        , Url.toString (Url.combine root (Route.toUrl Route.GoogleBadge))
        )
      , ("width", "200")
      , ("height", "51")
      ]
      []
  ]
