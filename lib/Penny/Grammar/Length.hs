{-# LANGUAGE TemplateHaskell #-}
module Penny.Grammar.Length where

import Control.Lens (makeLenses, over)
import Penny.Grammar

data Position = Position
  { _position :: !Int
  , _line :: !Int
  , _column :: !Int
  } deriving (Eq, Ord, Show)

makeLenses ''Position

type Counter a = a -> Position -> Position

nextChar :: Counter a
nextChar _ = over position succ . over column succ

-- None of the terminals may be a newline, so each of them
-- increments the position, line, and column by one.

commentChar :: Counter CommentChar
commentChar = nextChar

nonEscapedChar :: Counter NonEscapedChar
nonEscapedChar = nextChar

usCharNonDigit :: Counter USCharNonDigit
usCharNonDigit = nextChar

