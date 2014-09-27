module Penny.Tree.Time.Time2 where

import qualified Penny.Tree.Space as Space
import qualified Penny.Tree.Time.Time3 as Time3
import qualified Penny.Tree.Brace.Close as Close
import qualified Penny.Core.Seconds as Seconds
import qualified Penny.Core.TimeZoneOffset as TZO
import qualified Penny.Tree.Time.Error as Error
import Text.Parsec.Text
import Control.Applicative

data T
  = End Close.T
  | Space Space.T Time3.T
  | Time3 Time3.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = fmap End Close.parser
  <|> Space <$> Space.parser <*> Time3.parser
  <|> fmap Time3 Time3.parser

-- | Returns both seconds and a zone, or an error if appropriate.
-- Default values are used if the tree contains no values at all;
-- errors are returned if the tree contains bad data.
toCore :: T -> Either Error.T (Seconds.T, TZO.T)
toCore (End _) = Right (Seconds.zero, TZO.zero)
toCore (Space _ t3) = Time3.toCore t3
toCore (Time3 t3) = Time3.toCore t3
