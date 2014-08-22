{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Flag
  ( Flag
  , unFlag
  , toCopperFlag
  ) where

import Control.Applicative hiding (many)
import qualified Data.Text as X
import Data.Monoid
import Penny.Copper.Render
import qualified Penny.Common as P
import Text.Parsec

-- | A Flag, with no newlines and no closing square brace characters.
newtype Flag = Flag { unFlag :: P.Flag }
  deriving (Eq, Ord, Show)

toCopperFlag :: P.Flag -> Maybe Flag
toCopperFlag (P.Flag x)
  | X.all (not . banned) x = Just (Flag (P.Flag x))
  | otherwise = Nothing

banned :: Char -> Bool
banned c
  = c == '\n'
  || c == ']'

instance Renderable Flag where
  render (Flag (P.Flag x)) = "[" <> x <> "]"
  parser =
    char '['
    *> fmap (Flag . P.Flag . X.pack) (many (satisfy (not . banned)))
    <* char ']'
