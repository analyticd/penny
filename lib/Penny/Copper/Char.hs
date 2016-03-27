{-# LANGUAGE FlexibleContexts #-}
-- | Converting characters and strings to Copper types.  Many of
-- these functions can fail.
module Penny.Copper.Char where

import Penny.Copper.Singleton
import Penny.Copper.Types
import Penny.SeqUtil (convertHead)

import Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

fNonEscapedChar :: Char -> Maybe NonEscapedChar
fNonEscapedChar = Lens.preview _NonEscapedChar

fEscSeq :: Char -> Maybe EscSeq
fEscSeq c
  | c == '\\' = sq (EscPayload'Backslash sBackslash)
  | c == '\n' = sq (EscPayload'Newline sNewline)
  | c == '"' = sq (EscPayload'DoubleQuote sDoubleQuote)
  | otherwise = Nothing
  where
    sq p = Just $ EscSeq sBackslash p

fQuotedChar :: Char -> QuotedChar
fQuotedChar c = case fEscSeq c of
  Nothing -> QuotedChar'NonEscapedChar (NonEscapedChar c)
  Just s -> QuotedChar'EscSeq s

fQuotedChar'Seq :: Foldable c => c Char -> QuotedChar'Seq
fQuotedChar'Seq = QuotedChar'Seq . Seq.fromList . fmap fQuotedChar . toList

fQuotedString :: Foldable c => c Char -> QuotedString
fQuotedString x = QuotedString sDoubleQuote (fQuotedChar'Seq x) sDoubleQuote

fUnquotedStringNonDigitChar :: Char -> Maybe UnquotedStringNonDigitChar
fUnquotedStringNonDigitChar = Lens.preview _UnquotedStringNonDigitChar

fD0'9 :: Char -> Maybe D0'9
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

fD0'9'Seq :: Traversable c => c Char -> Maybe D0'9'Seq
fD0'9'Seq
  = fmap (D0'9'Seq . Seq.fromList . toList)
  . sequence
  . fmap fD0'9

fUnquotedStringNonFirstChar :: Char -> Maybe UnquotedStringNonFirstChar
fUnquotedStringNonFirstChar c
  = UnquotedStringNonFirstChar'UnquotedStringNonDigitChar
      <$> fUnquotedStringNonDigitChar c
  <|> UnquotedStringNonFirstChar'D0'9
      <$> fD0'9 c

fUnquotedStringNonFirstChar'Seq
  :: Traversable c
  => c Char
  -> Maybe UnquotedStringNonFirstChar'Seq
fUnquotedStringNonFirstChar'Seq
  = fmap (UnquotedStringNonFirstChar'Seq . Seq.fromList . toList)
  . sequence
  . fmap fUnquotedStringNonFirstChar

fUnquotedString :: Seq Char -> Maybe UnquotedString
fUnquotedString chars = do
  let (digits, rest) = getFirstDigits chars
  (firstNonDigitChar, rest') <- getFirstNonDigitChar rest
  rest'' <- fUnquotedStringNonFirstChar'Seq rest'
  return $ UnquotedString digits firstNonDigitChar rest''
  where
    getFirstDigits sq =
      let (digs, rest) = convertHead fD0'9 sq
      in (D0'9'Seq digs, rest)
    getFirstNonDigitChar sq = do
      (first, rest) <- Lens.uncons sq
      nonDigit <- fUnquotedStringNonDigitChar first
      return (nonDigit, rest)

fUnquotedStringNonDigitChar'Seq1
  :: Seq Char
  -> Maybe UnquotedStringNonDigitChar'Seq1
fUnquotedStringNonDigitChar'Seq1 sq = do
  (x, xs) <- Lens.uncons sq
  x' <- fUnquotedStringNonDigitChar x
  xs' <- sequence . fmap fUnquotedStringNonDigitChar $ xs
  return $ UnquotedStringNonDigitChar'Seq1 (x', xs')

fCommodity :: Seq Char -> Commodity
fCommodity sq = case fUnquotedStringNonDigitChar'Seq1 sq of
  Nothing -> Commodity'QuotedCommodity (QuotedCommodity (fQuotedString sq))
  Just us -> Commodity'UnquotedCommodity (UnquotedCommodity us)
