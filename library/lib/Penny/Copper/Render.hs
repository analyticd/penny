{-# LANGUAGE FlexibleInstances #-}
module Penny.Copper.Render where

import Penny.Numbers.Natural hiding (length)
import Data.Maybe (fromMaybe)
import Data.List (genericLength)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as X
import Text.Parsec.Text (Parser)
import Deka.Native.Abstract
import Text.Parsec.Pos
import Text.Parsec
import Data.Sequence (Seq, fromList)
import qualified Data.Foldable as F
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.Unpolar
import Data.Monoid
import Penny.Numbers.Abstract.RadGroup
import Data.Sums

-- | Things that can be rendered.  Properties of renderable items:
--
-- * 'render' is an injective function:
-- <http://en.wikipedia.org/wiki/Injective_function>
--
-- * 'render' never produces a null 'Text'
--
-- * The function
--
-- @
--   let run x = 'parser' x 'Control.Applicative.<*' 'Text.Parsec.Combinator.eof' in
--   'Data.Either.Combinators.fromRight' '.' 'Text.Parsec.Prim.parse' 'run' \"\"
-- @
--
-- is the left inverse of 'render'.
--
-- * 'parser' does not accept an empty string; that is, if 'parser'
-- succeeds, it consumes at least one character.

class Renderable a where
  render :: a -> Text
  parser :: Parser a

instance Renderable Novem where
  render = X.singleton . novemToChar
  parser = tokenPrim (:[]) (\ps t _ -> updatePosChar ps t)
    charToNovem

instance Renderable Decem where
  render = X.singleton . decemToChar
  parser = tokenPrim (:[]) (\ps t _ -> updatePosChar ps t)
    charToDecem

-- | Renders a sequence of renderable things.  Does not necessarily
-- obey the 'Renderable' laws, because the sequence might be empty.

renderSequence
  :: (Renderable a, F.Foldable f, Functor f)
  => f a
  -> Text
renderSequence = X.concat . F.toList . fmap render

-- | Parses a sequence of renderable things.

parseSequence :: Renderable a => Parser (Seq a)
parseSequence = fmap fromList $ many parser

instance Renderable NovDecs where
  render (NovDecs n ds) = render n <> renderSequence ds
  parser = liftM2 NovDecs parser parseSequence

instance Renderable ZeroesNovDecs where
  render (ZeroesNovDecs z nd) = renderNonNegZeroes z
    <> render nd
  parser = liftM2 ZeroesNovDecs parseNonNegZeroes parser

instance Renderable DecDecs where
  render (DecDecs d ds) = render d <> renderSequence ds
  parser = liftM2 DecDecs parser parseSequence


instance Renderable Zeroes where
  render (Zeroes p)
    | i > fromIntegral (maxBound :: Int) =
        error "zeroes: too many zeroes!"
    | otherwise = X.replicate (fromIntegral i) (X.singleton '0')
    where
      i = unPos p

  parser = fmap f $ many1 (char '0')
    where
      f = Zeroes . fromMaybe (error "zeroes: parser: error")
        . pos . genericLength


-- | Render zero or more zeroes.  Does not respect the Renderable laws
-- as the resulting list might be empty.
renderNonNegZeroes :: NonNeg -> Text
renderNonNegZeroes nn
  | i > fromIntegral (maxBound :: Int) =
      error "renderZeroes: too many zeroes!"
  | otherwise = X.replicate (fromIntegral i) (X.singleton '0')
  where
    i = unNonNeg nn

parseNonNegZeroes :: Parser NonNeg
parseNonNegZeroes = do
  zs <- many1 (char '0')
  maybe (error "parseNonNegZeroes: error") return
    . nonNeg . fromIntegral . length $ zs

instance Renderable UNWhole where
  render = render . unUNWhole
  parser = fmap UNWhole parser

instance Renderable Period where
  render _ = X.singleton ','
  parser = char ',' >> return Comma

instance Renderable Comma where
  render _ = X.singleton '.'
  parser = char '.' >> return Period

instance Renderable a => Renderable (Grouper a) where
  render a = case a of
    Space -> X.singleton ' '
    Thin -> X.singleton '\x2009'
    Under -> X.singleton '_'
    Unique b -> render b

  parser =
    (char ' ' >> return Space)
    <|> (char '\x2009' >> return Thin)
    <|> (char '_' >> return Under)
    <|> fmap Unique parser

instance Renderable (Radix Period) where
  render _ = X.singleton '.'
  parser = char '.' >> return radPeriod

instance Renderable (Radix Comma) where
  render _ = X.singleton ','
  parser = char ',' >> return radComma

instance (Renderable r, Renderable b) => Renderable (Group r b) where
  render (Group g p) = render g <> render p
  parser = liftM2 Group parser parser

instance Renderable (UNWholeRadix Period) where
  render (UNWholeRadix nd rdx md) =
    render nd <> render rdx <> maybe X.empty render md
  parser = liftM3 UNWholeRadix parser parser (optionMaybe parser)

instance Renderable (UNWholeRadix Comma) where
  render (UNWholeRadix nd rdx md) =
    render nd <> render rdx <> maybe X.empty render md
  parser = liftM3 UNWholeRadix parser parser (optionMaybe parser)

-- | HasZeroDigit does not follow 'Renderable' properties, as its
-- parser succeeds on the empty input, and its renderer may produce no
-- input

renderZeroDigit :: HasZeroDigit -> Text
renderZeroDigit (HasZeroDigit zd) | zd = X.singleton '0'
                                  | otherwise = X.empty

parseZeroDigit :: Parser HasZeroDigit
parseZeroDigit = fmap HasZeroDigit $ (char '0' >> return True)
                                     <|> return False

instance Renderable (UNRadFrac Period) where
  render (UNRadFrac z rd znd) = renderZeroDigit z
    <> render rd <> render znd

  parser = liftM3 UNRadFrac parseZeroDigit parser parser

instance Renderable (UNRadFrac Comma) where
  render (UNRadFrac z rd znd) = renderZeroDigit z
    <> render rd <> render znd

  parser = liftM3 UNRadFrac parseZeroDigit parser parser

instance Renderable UZZeroOnly where
  render _ = X.singleton '0'
  parser = char '0' >> return UZZeroOnly

instance Renderable (UZTrailing Period) where
  render (UZTrailing z r mz) = renderZeroDigit z <> render r <>
    maybe X.empty render mz

  parser = liftM3 UZTrailing parseZeroDigit parser (optionMaybe parser)

instance Renderable (UZTrailing Comma) where
  render (UZTrailing z r mz) = renderZeroDigit z <> render r <>
    maybe X.empty render mz

  parser = liftM3 UZTrailing parseZeroDigit parser (optionMaybe parser)


instance Renderable (GZ Period) where
  render (GZ hz r z g1 gs) = renderZeroDigit hz <> render r
    <> render z <> render g1 <> renderSequence gs

  parser = liftM5 GZ parseZeroDigit parser parser parser parseSequence

instance Renderable (GZ Comma) where
  render (GZ hz r z g1 gs) = renderZeroDigit hz <> render r
    <> render z <> render g1 <> renderSequence gs

  parser = liftM5 GZ parseZeroDigit parser parser parser parseSequence


instance Renderable (MasunoGroupedLeft Period) where
  render (MasunoGroupedLeft nd g1 gs) = render nd <> render g1
    <> renderSequence gs

  parser = liftM3 MasunoGroupedLeft parser parser parseSequence

instance Renderable (MasunoGroupedLeft Comma) where
  render (MasunoGroupedLeft nd g1 gs) = render nd <> render g1
    <> renderSequence gs

  parser = liftM3 MasunoGroupedLeft parser parser parseSequence

instance Renderable (MasunoGroupedLeftRad Period) where
  render (MasunoGroupedLeftRad mgl r my) =
    render mgl <> render r
    <> maybe X.empty (\(dd, sq) -> render dd <> renderSequence sq) my

  parser = liftM3 MasunoGroupedLeftRad parser parser
    (optionMaybe (liftM2 (,) parser parseSequence))

instance Renderable (MasunoGroupedLeftRad Comma) where
  render (MasunoGroupedLeftRad mgl r my) =
    render mgl <> render r
    <> maybe X.empty (\(dd, sq) -> render dd <> renderSequence sq) my

  parser = liftM3 MasunoGroupedLeftRad parser parser
    (optionMaybe (liftM2 (,) parser parseSequence))

instance Renderable (MasunoGroupedRight Comma) where
  render (MasunoGroupedRight nd rd dd g1 gs) =
    render nd <> render rd <> render dd <> render g1 <> renderSequence gs

  parser = liftM5 MasunoGroupedRight parser parser parser parser
    parseSequence

instance Renderable (MasunoGroupedRight Period) where
  render (MasunoGroupedRight nd rd dd g1 gs) =
    render nd <> render rd <> render dd <> render g1 <> renderSequence gs

  parser = liftM5 MasunoGroupedRight parser parser parser parser
    parseSequence

instance Renderable (FracunoFirstGroupZ Period) where
  render (FracunoFirstGroupZ hz rd z gz g gs) =
    renderZeroDigit hz <> render rd <> render z <> renderSequence gz
      <> render g <> renderSequence gs

  parser = FracunoFirstGroupZ
    `fmap` parseZeroDigit
    `ap` parser `ap` parser `ap` parseSequence `ap` parser
    `ap` parseSequence

instance Renderable (FracunoFirstGroupZ Comma) where
  render (FracunoFirstGroupZ hz rd z gz g gs) =
    renderZeroDigit hz <> render rd <> render z <> renderSequence gz
      <> render g <> renderSequence gs

  parser = FracunoFirstGroupZ
    `fmap` parseZeroDigit
    `ap` parser `ap` parser `ap` parseSequence `ap` parser
    `ap` parseSequence


instance Renderable (FracunoFirstGroupNZ Period) where
  render (FracunoFirstGroupNZ hz r znd g1 gs) =
    renderZeroDigit hz <> render r <> render znd <> render g1
      <> renderSequence gs

  parser = liftM5 FracunoFirstGroupNZ parseZeroDigit parser parser
    parser parseSequence

instance Renderable (FracunoFirstGroupNZ Comma) where
  render (FracunoFirstGroupNZ hz r znd g1 gs) =
    renderZeroDigit hz <> render r <> render znd <> render g1
      <> renderSequence gs

  parser = liftM5 FracunoFirstGroupNZ parseZeroDigit parser parser
    parser parseSequence

-- Aggregates

