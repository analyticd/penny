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
