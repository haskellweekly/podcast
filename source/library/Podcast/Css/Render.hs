module Podcast.Css.Render
  ( rules
  , rule
  , declarations
  , declaration
  )
where

import qualified Data.List as List
import qualified Podcast.Type.Css.Declaration as Declaration
import qualified Podcast.Type.Css.Rule as Rule

rules :: [Rule.Rule] -> String
rules = concatMap rule

rule :: Rule.Rule -> String
rule rule_ = concat
  [Rule.selector rule_, "{", declarations (Rule.declarations rule_), "}"]

declarations :: [Declaration.Declaration] -> String
declarations declarations_ =
  List.intercalate ";" (map declaration declarations_)

declaration :: Declaration.Declaration -> String
declaration declaration_ = concat
  [Declaration.property declaration_, ":", Declaration.value declaration_]
