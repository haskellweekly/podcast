module Podcast.Site.Feed
  ( rss
  )
where

import qualified Podcast.Type.Article as Article
import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Route as Route
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Title as Title
import qualified Podcast.Type.Url as Url
import qualified Podcast.Xml as Xml
import qualified Podcast.Xml.Render as Render

rss :: Url.Url -> [Episode.Episode] -> Xml.Element
rss root episodes = Xml.element
  "rss"
  [ ("version", "2.0")
  , ("xmlns:atom", "http://www.w3.org/2005/Atom")
  , ("xmlns:itunes", "http://www.itunes.com/dtds/podcast-1.0.dtd")
  ]
  [channel root episodes]

channel :: Url.Url -> [Episode.Episode] -> Xml.Node
channel root episodes = Xml.node
  "channel"
  []
  (channelAuthor
  : channelCategory
  : channelCopyright
  : channelDescription
  : channelExplicit
  : channelImage root
  : channelLanguage
  : channelLink root
  : channelOwner
  : channelSelfLink root
  : channelTitle
  : map (item root) episodes
  )

channelAuthor :: Xml.Node
channelAuthor = Xml.node "itunes:author" [] [Xml.text "Taylor Fausak"]

channelCategory :: Xml.Node
channelCategory = Xml.node
  "itunes:category"
  [("text", "Technology")]
  [Xml.node "itunes:category" [("text", "Tech News")] []]

channelCopyright :: Xml.Node
channelCopyright =
  Xml.node "copyright" [] [Xml.text "\xa9 2019 Taylor Fausak"]

channelDescription :: Xml.Node
channelDescription = Xml.node
  "description"
  []
  [ Xml.text
      "Haskell Weekly covers the Haskell progamming language. Listen to \
      \professional software developers discuss using functional programming \
      \to solve real-world business problems. Each episode uses a \
      \conversational two-host format and runs for about 15 minutes."
  ]

channelExplicit :: Xml.Node
channelExplicit = Xml.node "itunes:explicit" [] [Xml.text "clean"]

channelImage :: Url.Url -> Xml.Node
channelImage root = Xml.node
  "image"
  []
  [ Xml.node "title" [] [Xml.text "Haskell Weekly"]
  , Xml.node "link" [] [Xml.text (Url.toString root)]
  , Xml.node
    "url"
    []
    [Xml.text (Url.toString (Url.combine root (Route.toUrl Route.Logo)))]
  ]

channelLanguage :: Xml.Node
channelLanguage = Xml.node "language" [] [Xml.text "en-US"]

channelLink :: Url.Url -> Xml.Node
channelLink root = Xml.node "link" [] [Xml.text (Url.toString root)]

channelOwner :: Xml.Node
channelOwner = Xml.node
  "itunes:owner"
  []
  [ Xml.node "itunes:name" [] [Xml.text "Taylor Fausak"]
  , Xml.node "itunes:email" [] [Xml.text "taylor@fausak.me"]
  ]

channelSelfLink :: Url.Url -> Xml.Node
channelSelfLink root = Xml.node
  "atom:link"
  [ ("rel", "self")
  , ("href", Url.toString (Url.combine root (Route.toUrl Route.Feed)))
  , ("type", "application/rss+xml")
  ]
  []

channelTitle :: Xml.Node
channelTitle = Xml.node "title" [] [Xml.text "Haskell Weekly"]

item :: Url.Url -> Episode.Episode -> Xml.Node
item root episode = Xml.node
  "item"
  []
  [ itemAuthor
  , itemDescription episode
  , itemDuration episode
  , itemEnclosure episode
  , itemEpisode episode
  , itemGuid episode
  , itemLink root episode
  , itemPubDate episode
  , itemTitle episode
  ]

itemAuthor :: Xml.Node
itemAuthor = Xml.node "itunes:author" [] [Xml.text "Taylor Fausak"]

itemDescription :: Episode.Episode -> Xml.Node
itemDescription episode = Xml.node
  "description"
  []
  [ Xml.text
      (Render.nodes
        [ Xml.node
          "p"
          []
          [Xml.text (Description.toString (Episode.description episode))]
        , Xml.node
          "p"
          []
          [ Xml.node
              "a"
              [("href", Article.toString (Episode.article episode))]
              [Xml.text (Article.toString (Episode.article episode))]
          ]
        ]
      )
  ]

itemDuration :: Episode.Episode -> Xml.Node
itemDuration episode = Xml.node
  "itunes:duration"
  []
  [Xml.text (Seconds.toString (Episode.duration episode))]

itemEnclosure :: Episode.Episode -> Xml.Node
itemEnclosure episode = Xml.node
  "enclosure"
  [ ("type", "audio/mpeg")
  , ("length", Bytes.toString (Episode.size episode))
  , ("url", Media.toString (Episode.media episode))
  ]
  []

itemEpisode :: Episode.Episode -> Xml.Node
itemEpisode episode = Xml.node
  "itunes:episode"
  []
  [Xml.text (Number.toString (Episode.number episode))]

itemGuid :: Episode.Episode -> Xml.Node
itemGuid episode = Xml.node
  "guid"
  [("isPermaLink", "false")]
  [Xml.text (Guid.toString (Episode.guid episode))]

itemLink :: Url.Url -> Episode.Episode -> Xml.Node
itemLink root episode = Xml.node
  "link"
  []
  [ Xml.text
      (Url.toString
        (Url.combine
          root
          (Route.toUrl (Route.Episode (Episode.number episode)))
        )
      )
  ]

itemPubDate :: Episode.Episode -> Xml.Node
itemPubDate episode =
  Xml.node "pubDate" [] [Xml.text (Date.toRfc822 (Episode.date episode))]

itemTitle :: Episode.Episode -> Xml.Node
itemTitle episode =
  Xml.node "title" [] [Xml.text (Title.toString (Episode.title episode))]
