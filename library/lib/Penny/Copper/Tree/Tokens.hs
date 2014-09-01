module Penny.Copper.Tree.Tokens where

data OpenParen = OpenParen
  deriving (Eq, Ord, Show)

data CloseParen = CloseParen
  deriving (Eq, Ord, Show)

data Octothorpe = Octothorpe
  deriving (Eq, Ord, Show)

data Hyphen = Hyphen
  deriving (Eq, Ord, Show)

data Solidus = Solidus
  deriving (Eq, Ord, Show)

data AtSign = AtSign
  deriving (Eq, Ord, Show)

data Asterisk = Asterisk
  deriving (Eq, Ord, Show)

data Space = Space
  deriving (Eq, Ord, Show)

data OpenCurly = OpenCurly
  deriving (Eq, Ord, Show)

data CloseCurly = CloseCurly
  deriving (Eq, Ord, Show)

data Colon = Colon
  deriving (Eq, Ord, Show)

data Plus = Plus
  deriving (Eq, Ord, Show)

data OpenSquare = OpenSquare
  deriving (Eq, Ord, Show)

data CloseSquare = CloseSquare
  deriving (Eq, Ord, Show)

data Newline = Newline
  deriving (Eq, Ord, Show)

data Tilde = Tilde
  deriving (Eq, Ord, Show)

data LessThan = LessThan
  deriving (Eq, Ord, Show)

data GreaterThan = GreaterThan
  deriving (Eq, Ord, Show)

data Apostrophe = Apostrophe
  deriving (Eq, Ord, Show)

data Caret = Caret
  deriving (Eq, Ord, Show)

data Ampersand = Ampersand
  deriving (Eq, Ord, Show)

data Semicolon = Semicolon
  deriving (Eq, Ord, Show)

data EOF = EOF
  deriving (Eq, Ord, Show)
