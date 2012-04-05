module Penny.Brass.AlexInput where

import qualified Data.Text as X
import qualified Data.Text.Encoding as XE
import qualified Data.ByteString as BS
import Data.Word (Word8)

type AlexInput = BS.ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte bs =
  case BS.uncons bs of
    Nothing -> Nothing
    Just (w8, leftover) -> Just (w8, leftover)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ =
  error "Do not use left-context patterns in the Alex specification"

data Token =
  Spaces !Int
  | Newline
  | Exclamation
  | Quote
  | Hash
  | Dollar
  | Percent
  | Ampersand
  | Apostrophe
  | OpenParen
  | CloseParen
  | Asterisk
  | Plus
  | Comma
  | Dash
  | Period
  | Slash
  | Colon
  | Semicolon
  | LessThan
  | Equals
  | GreaterThan
  | Question
  | AtSign
  | OpenBracket
  | Backslash
  | CloseBracket
  | Caret
  | Underscore
  | Backtick
  | OpenBrace
  | VerticalBar
  | CloseBrace
  | Tilde
  | Dr
  | Debit
  | Cr
  | Credit
  | UpperLowerOther !X.Text
  | DigitsShort !X.Text
  | DigitsLong !X.Text
  | EOF
  deriving Show

upperLowerOther :: BS.ByteString -> Token
upperLowerOther = UpperLowerOther . XE.decodeUtf8

digitsShort :: BS.ByteString -> Token
digitsShort = DigitsShort . XE.decodeUtf8

digitsLong :: BS.ByteString -> Token
digitsLong = DigitsLong . XE.decodeUtf8

spaces :: BS.ByteString -> Token
spaces = Spaces . fromIntegral . BS.length
