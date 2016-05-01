{-# LANGUAGE FlexibleContexts #-}
-- | Converting characters and strings to Copper types.  Many of
-- these functions can fail.
module Penny.Copper.Char where

import Penny.Copper.Optics
import Penny.Copper.Singleton
import Penny.Copper.Types
import Penny.SeqUtil (convertHead)

import Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Pinchot

fNonEscapedChar :: Char -> Maybe (NonEscapedChar Char ())
fNonEscapedChar c = Lens.preview _NonEscapedChar (c, ())

fEscSeq :: Char -> Maybe (EscSeq Char ())
fEscSeq c
  | c == '\\' = sq (EscPayload'Backslash sBackslash)
  | c == '\n' = sq (EscPayload'Newline sNewline)
  | c == '"' = sq (EscPayload'DoubleQuote sDoubleQuote)
  | otherwise = Nothing
  where
    sq p = Just $ EscSeq sBackslash p

fQuotedChar :: Char -> (QuotedChar Char ())
fQuotedChar c = case fEscSeq c of
  Nothing -> QuotedChar'NonEscapedChar (NonEscapedChar (c, ()))
  Just s -> QuotedChar'EscSeq s

fQuotedChar'Star :: Foldable c => c Char -> QuotedChar'Star Char ()
fQuotedChar'Star = QuotedChar'Star . Seq.fromList . fmap fQuotedChar . toList

fQuotedString :: Foldable c => c Char -> QuotedString Char ()
fQuotedString x = QuotedString sDoubleQuote (fQuotedChar'Star x) sDoubleQuote

fUnquotedStringNonDigitChar
  :: Char
  -> Maybe (UnquotedStringNonDigitChar Char ())
fUnquotedStringNonDigitChar c
  = Lens.preview _UnquotedStringNonDigitChar (c, ())

fD0'9 :: Char -> Maybe (D0'9 Char ())
fD0'9 c
  | c == '0' = Just (D0'9'Zero sZero)
  | c == '1' = Just (D0'9'One sOne)
  | c == '2' = Just (D0'9'Two sTwo)
  | c == '3' = Just (D0'9'Three sThree)
  | c == '4' = Just (D0'9'Four sFour)
  | c == '5' = Just (D0'9'Five sFive)
  | c == '6' = Just (D0'9'Six sSix)
  | c == '7' = Just (D0'9'Seven sSeven)
  | c == '8' = Just (D0'9'Eight sEight)
  | c == '9' = Just (D0'9'Nine sNine)
  | otherwise = Nothing

fD0'9'Star :: Traversable c => c Char -> Maybe (D0'9'Star Char ())
fD0'9'Star
  = fmap (D0'9'Star . Seq.fromList . toList)
  . sequence
  . fmap fD0'9

fUnquotedStringNonFirstChar
  :: Char
  -> Maybe (UnquotedStringNonFirstChar Char ())
fUnquotedStringNonFirstChar c
  = UnquotedStringNonFirstChar'UnquotedStringNonDigitChar
      <$> fUnquotedStringNonDigitChar c
  <|> UnquotedStringNonFirstChar'D0'9
      <$> fD0'9 c

fUnquotedStringNonFirstChar'Star
  :: Traversable c
  => c Char
  -> Maybe (UnquotedStringNonFirstChar'Star Char ())
fUnquotedStringNonFirstChar'Star
  = fmap (UnquotedStringNonFirstChar'Star . Seq.fromList . toList)
  . sequence
  . fmap fUnquotedStringNonFirstChar

fUnquotedString :: Seq Char -> Maybe (UnquotedString Char ())
fUnquotedString chars = do
  let (digits, rest) = getFirstDigits chars
  (firstNonDigitChar, rest') <- getFirstNonDigitChar rest
  rest'' <- fUnquotedStringNonFirstChar'Star rest'
  return $ UnquotedString digits firstNonDigitChar rest''
  where
    getFirstDigits sq =
      let (digs, rest) = convertHead fD0'9 sq
      in (D0'9'Star digs, rest)
    getFirstNonDigitChar sq = do
      (first, rest) <- Lens.uncons sq
      nonDigit <- fUnquotedStringNonDigitChar first
      return (nonDigit, rest)

fString :: Seq Char -> Either (UnquotedString Char ()) (QuotedString Char ())
fString cs = case fUnquotedString cs of
  Just s -> Left s
  Nothing -> Right . fQuotedString $ cs

fUnquotedStringNonDigitChar'Plus
  :: Seq Char
  -> Maybe (UnquotedStringNonDigitChar'Plus Char ())
fUnquotedStringNonDigitChar'Plus sq = do
  (x, xs) <- Lens.uncons sq
  x' <- fUnquotedStringNonDigitChar x
  xs' <- sequence . fmap fUnquotedStringNonDigitChar $ xs
  return $ UnquotedStringNonDigitChar'Plus (Pinchot.NonEmpty x' xs')

fCommodity :: Seq Char -> Commodity Char ()
fCommodity sq = case fUnquotedStringNonDigitChar'Plus sq of
  Nothing -> Commodity'QuotedCommodity (QuotedCommodity (fQuotedString sq))
  Just us -> Commodity'UnquotedCommodity (UnquotedCommodity us)
