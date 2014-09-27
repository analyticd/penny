module Penny.Tree.Time.Zone where

import qualified Penny.Tree.Hyphen as Hyphen
import qualified Penny.Tree.Plus as Plus
import qualified Penny.Tree.Digits.D4 as D4
import qualified Penny.Tree.Brace.Close as Close
import qualified Penny.Tree.Time.Error as Error
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Core.TimeZoneOffset as TimeZoneOffset

data T = T (Either Hyphen.T Plus.T) D4.T Close.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> (fmap Left Hyphen.parser <|> fmap Right Plus.parser)
  <*> D4.parser <*> Close.parser

toCore :: T -> Either Error.T TimeZoneOffset.T
toCore (T ei d4 _) = maybe (Left $ Error.BadOffset ei d4)
  Right . TimeZoneOffset.fromInt . changeSign . D4.toInt $ d4
  where
    changeSign = either (const negate) (const id) ei
