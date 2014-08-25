module Penny.Copper.UnpolarOld where

import Penny.Copper.Render

import Penny.Numbers.Natural hiding (length)
import Penny.Numbers.Abstract.Aggregates
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
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.Unpolar
import Data.Monoid
import Penny.Numbers.Abstract.RadGroup
import Data.Sums

{-

Unpolar
  UngroupedUnpolar
    UngroupedZero
      UZZeroOnly
      UZTrailing
        HasZeroDigit * Radix * Maybe Zeroes
    UngroupedNonZero
      UNWhole
        NovDecs
      UNWholeRadix
        NovDecs * Radix * Maybe DecDecs
      UNRadFrac
        HasZeroDigit * Radix * (NonNegZeroes * NovDecs)
  GroupedUnpolar
    GZ
      HasZeroDigit * Radix * Zeroes * (Group Zeroes) * (Seq (Group Zeroes))
    GroupedNonZero
      MasunoGroupedLeft
        NovDecs * (Group DecDecs) * (Seq (Group DecDecs))
      MasunoGroupedLeftRad
        MasunoGroupedLeft * Radix * Maybe (DecDecs, Seq (Group DecDecs))
      MasunoGroupedRight
        NovDecs * Radix * DecDecs * Group DecDecs * Seq (Group DecDecs)
      FracunoFirstGroupZ
        HasZeroDigit * Radix * Zeroes * Seq (Group Zeroes)
          * Group ZeroesNovDecs * Seq (Group DecDecs)
      FracunoFirstGroupNZ
        HasZeroDigit * Radix * ZeroesNovDecs * Group DecDecs
          * Seq (Group DecDecs)
-}

{-

NovDecs First:

UNWhole
  NovDecs
UNWholeRadix
  NovDecs * Radix * Maybe DecDecs
MasunoGroupedRight
  NovDecs * Radix * DecDecs * Group DecDecs * Seq (Group DecDecs)

MasunoGroupedLeft
  NovDecs * (Group DecDecs) * (Seq (Group DecDecs))
MasunoGroupedLeftRad
  MasunoGroupedLeft * Radix * Maybe (DecDecs, Seq (Group DecDecs))

Mandatory Zero First:

UZZeroOnly

Optional Zero First:

UZTrailing
  HasZeroDigit * Radix * Maybe Zeroes
UNRadFrac
  HasZeroDigit * Radix * (NonNegZeroes * NovDecs)
FracunoFirstGroupNZ
  HasZeroDigit * Radix * (NonNegZeroes * NovDecs) * Group DecDecs
    * Seq (Group DecDecs)

GZ
  HasZeroDigit * Radix * Zeroes * (Group Zeroes) * (Seq (Group Zeroes))
FracunoFirstGroupZ
  HasZeroDigit * Radix * Zeroes * Seq (Group Zeroes)
    * Group ZeroesNovDecs * Seq (Group DecDecs)
-}

novDecsFirst
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (S5 UNWhole
                (UNWholeRadix r)
                (MasunoGroupedRight r)
                (MasunoGroupedLeft r)
                (MasunoGroupedLeftRad r))
novDecsFirst pr pg = liftM2 f parser after
  where
    f nd rest = case rest of
      Nothing -> S5a $ UNWhole nd
      Just a -> a nd
    after = optionMaybe
      ( novDecsFirstMasunoGroupedLeft pr pg
        <|> novDecsFirstRadix pr pg )

novDecsFirstMasunoGroupedLeft
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (NovDecs -> S5 a b c (MasunoGroupedLeft r) (MasunoGroupedLeftRad r))
novDecsFirstMasunoGroupedLeft pr pg = do
  g1 <- pg
  gs <- parseSequence pg
  let mgl nd = MasunoGroupedLeft nd g1 gs
  mayRdx <- optionMaybe pr
  case mayRdx of
    Nothing -> return $ \nd -> S5d (mgl nd)
    Just rdx -> do
      next <- optionMaybe $ liftM2 (,) parser (parseSequence pg)
      return $ \nd -> S5e $ MasunoGroupedLeftRad (mgl nd) rdx next

novDecsFirstRadix
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (NovDecs -> S5 a (UNWholeRadix r) (MasunoGroupedRight r) d e)
novDecsFirstRadix pr pg = do
  rdx <- pr
  mayDD <- optionMaybe parser
  case mayDD of
    Nothing -> return $ \nd -> S5b $ UNWholeRadix nd rdx Nothing
    Just dd -> do
      mayGr <- optionMaybe pg
      case mayGr of
        Nothing -> return $ \nd -> S5b $ UNWholeRadix nd rdx (Just dd)
        Just gr -> do
          sq <- parseSequence pg
          return $ \nd -> S5c $ MasunoGroupedRight nd rdx dd gr sq

--
--
--

optionalZeroFirst
  :: Parser (Radix r)
  -> Parser (Group r Zeroes)
  -> Parser (Group r ZeroesNovDecs)
  -> Parser (Group r DecDecs)
  -> Parser (S6 UZZeroOnly
                (UZTrailing r)
                (UNRadFrac r)
                (GZ r)
                (FracunoFirstGroupZ r)
                (FracunoFirstGroupNZ r))
optionalZeroFirst pr pz pznd pd = undefined

-- | Parses items that lead with an optional zero, followed by a
-- mandatory radix.  The optional zero must already have been parsed.
--
-- First, see if there are zeroes after the radix.  If there are NO
-- zeroes, this is a 'UZTrailing', 'UNRadFrac', or
-- 'FracunoFirstGroupNZ'.  To determine which, see if any 'NovDecs'
-- follow the radix.  If no 'NovDecs' follow, this is a 'UZTrailing'.
-- Otherwise, if 'NovDecs' follow, see if a group of 'DecDecs'
-- follows.  If so, this is a 'FracunoFirstGroupNZ'; otherwise, it is
-- a 'UNRadFrac'.
--
-- If there ARE zeroes after the radix, nothing has been narrowed
-- down.  See if there are 'NovDecs' next.  If so, this is a
-- 'UNRadFrac' or a 'FracunoFirstGroupNZ'; which can be determined by
-- seeing if 'DecDecs' follow.  If instead a group of zeroes follows,
-- parse any remaining groups of zeroes.  If a group of
-- 'ZeroesNovDecs' follows, this is a 'FracunoFirstGroupZ'; otherwise,
-- it is a 'GZ'.
--
-- If neither a 'NovDecs' nor a group of zeroes follows the zeroes
-- after the radix, this is a 'UZTrailing'.
zeroThenRadix
  :: Parser (Radix r)
  -> Parser (Group r Zeroes)
  -> Parser (Group r ZeroesNovDecs)
  -> Parser (Group r DecDecs)
  -> Parser (HasZeroDigit -> S6 a
                (UZTrailing r)
                (UNRadFrac r)
                (GZ r)
                (FracunoFirstGroupZ r)
                (FracunoFirstGroupNZ r))
zeroThenRadix pr pg pznd pd = do
  rdx <- pr
  mayZs <- optionMaybe parser
  case mayZs of
    Nothing -> fmap ($ rdx) $ optZeroThenRadixNoZeroAfterRadix pd

-- Start here
    Just zs -> do
      mayNd <- optionMaybe parser
      case mayNd of
        Just nd -> do
          g1 <- pd
          gs <- parseSequence pd
          let znd = ZeroesNovDecs (posToNonNeg . unZeroes $ zs) nd
          return $ \hzd -> S6f $ FracunoFirstGroupNZ hzd rdx znd g1 gs

        Nothing -> do
          mayGzs <- optionMaybe pg
          case mayGzs of
            Nothing -> do
              znd <- pznd
              dds <- parseSequence pd
              return $ \hzd -> S6e $ FracunoFirstGroupZ hzd rdx zs S.empty
                znd dds

            Just gzs -> do
              zeroesGroups <- parseSequence pg
              mayZnd <- optionMaybe pznd
              case mayZnd of
                Nothing -> return $ \hzd -> S6d $ GZ hzd rdx zs gzs zeroesGroups
                Just znd -> undefined

optZeroThenRadixNoZeroAfterRadix
  :: Parser (Group r DecDecs)
  -> Parser (Radix r -> HasZeroDigit
              -> S6 a (UZTrailing r) (UNRadFrac r) d
                      e (FracunoFirstGroupNZ r))
optZeroThenRadixNoZeroAfterRadix pd = fmap f runParse
  where
    f mayNd = case mayNd of
      Nothing -> \rdx hzd -> S6b $ UZTrailing hzd rdx Nothing
      Just (nd, mayDD) -> case mayDD of
        Nothing -> \rdx hzd -> S6c $ UNRadFrac hzd rdx znd
        Just (dd, sq) -> \rdx hzd ->
          S6f $ FracunoFirstGroupNZ hzd rdx znd dd sq
        where
          znd = ZeroesNovDecs zeroNonNeg nd

    runParse = optionMaybe
      (liftM2 (,) parser (optionMaybe (liftM2 (,) pd (parseSequence pd))))

optZeroThenRadixZeroesAfterRadix
  :: Parser (Group r DecDecs)
  -> Parser (Group r Zeroes)
  -> Parser (Radix r -> HasZeroDigit
              -> S6 a (UZTrailing r) (UNRadFrac r)
                    (GZ r) (FracunoFirstGroupZ r)
                    (FracunoFirstGroupNZ r))
optZeroThenRadixZeroesAfterRadix pr pg = fmap f (parseZeroesAfterRadix pr pg)
  where
    f = undefined

parseZeroesAfterRadix = undefined

data Zero = Zero
  deriving (Eq, Ord, Show)

data Tree r =
  S1
    ( NovDecs
    , S2
        ( Radix r
        , ( S2
              ( DecDecs
              , S2
                  (Group r DecDecs, Seq (Group r DecDecs) )
                  S0
              )
              S0
          )
        )

        ( NovDecs
        , Group r DecDecs
        , Seq (Group r DecDecs)
        , S2
            ( Radix r
            , S2
                (DecDecs, Seq (Group r DecDecs))
                S0
            )
            S0
        )
    )

  deriving (Eq, Ord, Show)

