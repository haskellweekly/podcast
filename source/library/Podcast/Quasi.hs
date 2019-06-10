module Podcast.Quasi
  ( string
  )
where

import qualified Language.Haskell.TH.Quote as Quote
import qualified Language.Haskell.TH.Syntax as Syntax

string :: Quote.QuasiQuoter
string = Quote.QuasiQuoter
  { Quote.quoteDec = const
    (fail "cannot use [string|...|] quasi-quotation as a declaration")
  , Quote.quoteExp = pure . Syntax.LitE . Syntax.StringL
  , Quote.quotePat = pure . Syntax.LitP . Syntax.StringL
  , Quote.quoteType = pure . Syntax.LitT . Syntax.StrTyLit
  }
