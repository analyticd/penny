module Penny.Preset where

import Control.Lens (set)
import qualified Data.Sequence as Seq
import Penny.Clatch
import Penny.Clatcher
import Penny.Scheme
import Penny.Stream
import Penny.Columns
import Penny.Shortcut

-- | Sends output to @less@ in color, using a light background.

coless :: Clatcher r l
coless
  = set output (Seq.singleton (stream toLess))
  . set colors light
  $ mempty

-- | Sends output to @less@ in color; in addition, shows a
-- standardized register report.  The report shows, in this order:
--
-- * date
--
-- * number
--
-- * flag
--
-- * payee
--
-- * account
--
-- * troika
--
-- * balance

register :: Clatcher Columns l
register = set report rpt coless
  where
    rpt = Seq.fromList
      [ column date
      , column number
      , column flag
      , column payee
      , column account
      , column (posting . core . troika)
      , column balance
      ]
