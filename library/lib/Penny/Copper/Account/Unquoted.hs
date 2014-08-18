{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Account.Unquoted
  ( UnquotedAccountSpc
  , accountToUnquotedAccountSpc
  , unUnquotedAccountSpc
  , UnquotedAccountNL
  , accountToUnquotedAccountNL
  , unUnquotedAccountNL
  ) where

import Control.Applicative hiding (many)
import Data.Sequence
import Data.List (intersperse)
import qualified Data.Text as X
import Data.Char
import qualified Data.Foldable as F
import Penny.Posting
import Penny.Copper.Render
import Text.Parsec
import Text.Parsec.Text (Parser)

-- | An unquoted account appears with no surrounding quotes.  The
-- first character of the first sub-account must be a letter.  The
-- account is terminated by a space.  The account name must
-- not be empty.

newtype UnquotedAccountSpc
  = UnquotedAccountSpc { unUnquotedAccountSpc :: Account }
  deriving (Eq, Ord, Show)

-- | An unquoted account appears with no surrounding quotes.  The
-- first character of the first sub-account must be a letter.  The
-- account is terminated by a newline.  The account name must
-- not be empty.

newtype UnquotedAccountNL = UnquotedAccountNL
  { unUnquotedAccountNL :: Account }
  deriving (Eq, Ord, Show)

goodAccount :: Account -> Bool
goodAccount (Account as) = case viewl as of
  EmptyL -> False
  SubAccount x :< _ -> goodFirstLetter && goodRest
    where
      goodFirstLetter = case X.uncons x of
        Nothing -> False
        Just (l, _) -> isLetter l
      goodRest = F.all (X.all (not . banned)) . fmap unSubAccount $ as

accountToUnquotedAccountSpc :: Account -> Maybe UnquotedAccountSpc
accountToUnquotedAccountSpc a
  | goodAccount a = Just (UnquotedAccountSpc a)
  | otherwise = Nothing

accountToUnquotedAccountNL :: Account -> Maybe UnquotedAccountNL
accountToUnquotedAccountNL a
  | goodAccount a = Just (UnquotedAccountNL a)
  | otherwise = Nothing

banned :: Char -> Bool
banned c
  = c == ':'
  || c == ' '
  || c == '\n'

renderAccount :: Account -> X.Text
renderAccount = X.concat . intersperse ":" . F.toList
    . fmap unSubAccount . unAccount

parseAccount :: Parser Account
parseAccount =
  (Account . fromList)
  <$> ((:) <$> firstSubAccount <*> many nextSubAccount)
  where
    nextSubAccount = char ':' *> nonFirstSubAccount

firstSubAccount :: Parser SubAccount
firstSubAccount =
  (SubAccount . X.pack)
  <$> ((:) <$> letter <*> many (satisfy (not . banned)))

nonFirstSubAccount :: Parser SubAccount
nonFirstSubAccount =
  (SubAccount . X.pack)
  <$> (many (satisfy (not . banned)))

instance Renderable UnquotedAccountSpc where
  render = flip X.snoc ' ' . renderAccount . unUnquotedAccountSpc
  parse = fmap UnquotedAccountSpc $ parseAccount <* char ' '

instance Renderable UnquotedAccountNL where
  render = flip X.snoc '\n' . renderAccount . unUnquotedAccountNL
  parse = fmap UnquotedAccountNL $ parseAccount <* char '\n'
