module Penny.Copper.Util where

import Control.Applicative ((<*), pure, (<$))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as X
import Data.Ix (range)
import qualified Data.Set as S
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.TextNonEmpty as TNE
import Text.Parsec (char, many)
import Text.Parsec.Text (Parser)

inCat :: C.GeneralCategory -> C.GeneralCategory
         -> Char -> Bool
inCat g1 g2 c = C.generalCategory c `S.member` gs where
  gs = S.fromList (range (g1, g2))

-- | Creates a new parser that behaves like the old one, but also
-- parses any whitespace remaining afterward.
lexeme :: Parser a -> Parser a
lexeme p = p <* many (char ' ')

-- | Parses any trailing whitespace followed by a newline followed by
-- additional whitespace.
eol :: Parser ()
eol = pure ()
      <* many (char ' ')
      <* char '\n'
      <* many (char ' ')

-- | Parses a run of spaces.
spaces :: Parser ()
spaces = () <$ many (char ' ')

-- | Errors from rendering.
data RenderError = BadChar Char 
                   deriving Show

-- | Supplied with a function which returns True if a character is
-- allowed, returns a function that when given a character will return
-- a RenderError if the character is bad or the character if the
-- character is good.
checkChar :: (Char -> Bool)
             -> Char
             -> Ex.Exceptional RenderError Char
checkChar p c = if p c then return c else Ex.throw (BadChar c)

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
  -> Ex.Exceptional RenderError b
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
    Left c -> Ex.throw (BadChar c)
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
