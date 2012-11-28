module Penny.Copper.Util where

import Control.Applicative ((<*), pure, (<$))
import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as X
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.TextNonEmpty as TNE
import Text.Parsec (char, many, skipMany)
import Text.Parsec.Text (Parser)

rangeLettersToSymbols :: Char -> Bool
rangeLettersToSymbols c = case C.generalCategory c of
  C.UppercaseLetter -> True
  C.LowercaseLetter -> True
  C.TitlecaseLetter -> True
  C.ModifierLetter -> True
  C.OtherLetter -> True
  C.DecimalNumber -> True
  C.LetterNumber -> True
  C.OtherNumber -> True
  C.ConnectorPunctuation -> True
  C.DashPunctuation -> True
  C.OpenPunctuation -> True
  C.ClosePunctuation -> True
  C.InitialQuote -> True
  C.FinalQuote -> True
  C.OtherPunctuation -> True
  C.MathSymbol -> True
  C.CurrencySymbol -> True
  C.ModifierSymbol -> True
  C.OtherSymbol -> True
  _ -> False

rangeLetters :: Char -> Bool
rangeLetters c = case C.generalCategory c of
  C.UppercaseLetter -> True
  C.LowercaseLetter -> True
  C.TitlecaseLetter -> True
  C.ModifierLetter -> True
  C.OtherLetter -> True
  _ -> False

rangeMathCurrency :: Char -> Bool
rangeMathCurrency c = case C.generalCategory c of
  C.MathSymbol -> True
  C.CurrencySymbol -> True
  _ -> False

rangeSymbols :: Char -> Bool
rangeSymbols c = case C.generalCategory c of
  C.MathSymbol -> True
  C.CurrencySymbol -> True
  C.ModifierSymbol -> True
  C.OtherSymbol -> True
  _ -> False

rangeLettersNumbers :: Char -> Bool
rangeLettersNumbers c = case C.generalCategory c of
  C.UppercaseLetter -> True
  C.LowercaseLetter -> True
  C.TitlecaseLetter -> True
  C.ModifierLetter -> True
  C.OtherLetter -> True
  C.DecimalNumber -> True
  C.LetterNumber -> True
  C.OtherNumber -> True
  _ -> False

-- | Creates a new parser that behaves like the old one, but also
-- parses any whitespace remaining afterward.
lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany (char ' ')

-- | Parses any trailing whitespace followed by a newline followed by
-- additional whitespace.
eol :: Parser ()
eol = pure ()
      <* skipMany (char ' ')
      <* char '\n'
      <* skipMany (char ' ')

-- | Parses a run of spaces.
spaces :: Parser ()
spaces = () <$ many (char ' ')

-- | Applied to a non-empty list of pairs, with the first element of
-- the pair being a predicate that returns True if a character is OK
-- and the second element being something of an arbitrary type, and to
-- something that has a Text. The pairs must be ordered from most
-- restrictive to least restrictive predicates. If at least one of the
-- predicates indicates that the Text is valid, returns the leftmost b
-- associated with that predicate. If none of the predicates indicates
-- that the Text is valid, returns the rightmost error.
--
-- Here, most restrictive means the predicate that indicates True for
-- the narrowest range of characters, while least restrictive means
-- the predicate that indicates True for the widest range of
-- characters.
checkText ::
  HT.HasText a
  => NE.NonEmpty ((Char -> Bool), b)
  -> a
  -> Maybe b
checkText ps a = let
  t = HT.text a
  results = fmap (g . f) ps where
    f (p, b) = (X.find (not . p) t, b)
    g (p, b) = case p of
      Nothing -> Right b
      Just c -> Left c
  folder x y = case x of
    Right b -> Right b
    Left _ -> y
  in case F.foldr1 folder results of
    Left _ -> Nothing
    Right b -> return b

listIsOK ::
  HT.HasTextNonEmptyList a
  => (Char -> Bool) -- ^ Returns True for characters that are allowed
  -> a
  -> Bool
listIsOK p = F.all (TNE.all p) . HT.textNonEmptyList

firstCharOfListIsOK ::
  HT.HasTextNonEmptyList a
  => (Char -> Bool) -- ^ Returns True if the first character is allowed
  -> a
  -> Bool
firstCharOfListIsOK p ls = let
  firstText = NE.head . HT.textNonEmptyList $ ls
  in p (TNE.first firstText)

-- | Takes a field that may or may not be present and a function that
-- renders it. If the field is not present at all, returns an empty
-- Text. Otherwise will succeed or fail depending upon whether the
-- rendering function succeeds or fails.
renMaybe :: Maybe a -> (a -> Maybe X.Text) -> Maybe X.Text
renMaybe mx f = case mx of
  Nothing -> Just X.empty
  Just a -> f a

-- | Merges a list of words into one Text; however, if any given Text
-- is empty, that Text is first dropped from the list.
txtWords :: [X.Text] -> X.Text
txtWords xs = case filter (not . X.null) xs of
  [] -> X.empty
  rs -> X.unwords rs
