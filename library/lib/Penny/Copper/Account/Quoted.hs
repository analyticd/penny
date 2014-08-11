{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Account.Quoted where

import Control.Applicative hiding (many)
import Data.List (intersperse)
import Penny.Posting
import qualified Data.Foldable as F
import qualified Data.Text as X
import Penny.Copper.Render
import Text.Parsec.Text (Parser)
import Text.Parsec
import qualified Data.Sequence as S

banned :: Char -> Bool
banned c
  = c == ':'
  || c == '}'
  || c == '\n'

-- | A quoted account appears in the file surrounded by curly braces.
-- Each sub-account name cannot contain a colon, closing curly brace,
-- or newline.  The name can be entirely empty.

newtype QuotedAccount = QuotedAccount { unQuotedAccount :: Account }
  deriving (Eq, Ord, Show)

accountToQuotedAccount :: Account -> Maybe QuotedAccount
accountToQuotedAccount a@(Account as)
  | F.all good as = Just $ QuotedAccount a
  | otherwise = Nothing
  where
    good (SubAccount x) = X.all (not . banned) x

instance Renderable QuotedAccount where
  render = flip X.snoc '}' . X.cons '{'
    . X.concat . intersperse ":" . F.toList
    . fmap unSubAccount . unAccount . unQuotedAccount

  parse =
    (QuotedAccount . Account . S.fromList)
    <$ char '{'
    <*> sepBy subAccount (char ':')
    <* char '}'
    <?> "account"

subAccount :: Parser SubAccount
subAccount =
  (SubAccount . X.pack)
  <$> (many (satisfy (not . banned) <?> "sub-account letter"))
  <?> "sub-account"

