module Podcast.Css
  ( Rule.Rule
  , render
  , rule
  )
where

import qualified Podcast.Css.Build as Build
import qualified Podcast.Css.Render as Render
import qualified Podcast.Type.Css.Rule as Rule

render :: [Rule.Rule] -> String
render = Render.rules

rule :: String -> [(String, String)] -> Rule.Rule
rule = Build.rule
