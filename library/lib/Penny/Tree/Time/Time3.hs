module Penny.Tree.Time.Time3 where

import qualified Penny.Tree.Time.Error as Error
import qualified Penny.Tree.Colon as Colon
import qualified Penny.Tree.Digits.D1or2 as D2
import qualified Penny.Tree.Time.Zone as Zone
import qualified Penny.Tree.Time.Time4 as Time4
import qualified Penny.Core.TimeZoneOffset as TZO
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Core.Seconds as Seconds

data T
  = Seconds Colon.T D2.T Time4.T
  | Zone Zone.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Seconds <$> Colon.parser <*> D2.parser <*> Time4.parser
  <|> Zone <$> Zone.parser

-- | Returns both seconds and a zone, or an error if appropriate.
-- Default values are used if the tree contains no values at all;
-- errors are returned if the tree contains bad data.
toCore :: T -> Either Error.T (Seconds.T, TZO.T)
toCore (Seconds _ d2 t4) =
  (,)
  <$> maybe (Left . Error.BadSeconds $ d2) Right
            (Seconds.fromInt . D2.toInt $ d2)
  <*> Time4.toCore t4

toCore (Zone z) = (,) <$> pure Seconds.zero <*> Zone.toCore z
