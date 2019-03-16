module Podcast.Html
  ( Xml.Root
  , Xml.Node
  , Xml.root
  , Xml.node
  , Xml.text
  , render
  )
where

import qualified Data.Text as Text
import qualified Podcast.Xml as Xml

render :: Xml.Root -> String
render root = "<!doctype html>" <> Text.unpack (Xml.renderRoot root)
