module Penny.Copper.Account.Unquoted where

import Data.Sequence
import qualified Data.Text as X
import Data.Char
import qualified Data.Foldable as F
import Penny.Posting

-- | An unquoted account appears with no surrounding quotes.  The
-- first character of the first sub-account must be a letter.
-- Subsequent letters may not be a colon, space, or newline.  The
-- account name must not be empty.

newtype UnquotedAccount
  = UnquotedAccount { unUnquotedAccount :: Account }
  deriving (Eq, Ord, Show)

accountToUnquotedAccount :: Account -> Maybe UnquotedAccount
accountToUnquotedAccount a@(Account as) = case viewl as of
  EmptyL -> Nothing
  SubAccount x :< _
    | goodFirstLetter && goodRest -> Just $ UnquotedAccount a
    | otherwise -> Nothing
    where
      goodFirstLetter = case X.uncons x of
        Nothing -> False
        Just (l, _) -> isLetter l
      goodRest = F.all (X.all (not . banned)) . fmap unSubAccount $ as

banned :: Char -> Bool
banned c
  = c == ':'
  || c == ' '
  || c == '\n'
