module Penny.Tree.Time.Time4 where

import qualified Penny.Tree.Time.Zone as Zone
import qualified Penny.Tree.Space as Space
import qualified Penny.Tree.Brace.Close as Close
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Tree.Time.Error as Error
import qualified Penny.Core.TimeZoneOffset as TZO

-- | May contain a time zone.
data T
  = Zone Zone.T
  | Space Space.T Zone.T
  | Close Close.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Zone <$> Zone.parser
  <|> Space <$> Space.parser <*> Zone.parser
  <|> Close <$> Close.parser

-- | If the production contains only a closing brace, returns a zero
-- offset; otherwise, returns the offset given (with an error if the
-- user entered a bad offset.)
toCore :: T -> Either Error.T TZO.T
toCore (Zone z) = Zone.toCore z
toCore (Space _ z) = Zone.toCore z
toCore (Close _) = return TZO.zero
