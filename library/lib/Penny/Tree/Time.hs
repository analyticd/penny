module Penny.Tree.Time where

import qualified Penny.Tree.Brace.Open as Open
import qualified Penny.Tree.Colon as Colon
import qualified Penny.Tree.Digits.D1or2 as D2
import qualified Penny.Tree.Time.Time2 as Time2
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Core.Hours as Hours
import qualified Penny.Core.Minutes as Minutes
import qualified Penny.Core.Seconds as Seconds
import qualified Penny.Core.TimeZoneOffset as TZO
import qualified Penny.Tree.Time.Error as Error

data T = T Open.T D2.T Colon.T D2.T Time2.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Open.parser <*> D2.parser <*> Colon.parser
  <*> D2.parser <*> Time2.parser

-- | Returns default values for values not provided, or an error if
-- bad data is provided.

toCore :: T -> Either Error.T (Hours.T, Minutes.T, Seconds.T, TZO.T)
toCore (T _ hr _ mi t2) =
  f <$> maybe (Left $ Error.BadHours hr) Right
        (Hours.fromInt . D2.toInt $ hr)
    <*> maybe (Left $ Error.BadMinutes mi) Right
        (Minutes.fromInt . D2.toInt $ mi)
    <*> Time2.toCore t2
  where
    f hrs mis (secs, tzo) = (hrs, mis, secs, tzo)
