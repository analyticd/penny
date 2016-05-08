module Penny.Copper.Freezer where

import Penny.Amount
import Penny.Copper.Copperize
import Penny.Copper.PriceParts
import Penny.Copper.Types
import qualified Penny.Tree as Tree
import qualified Penny.Scalar as Scalar
import qualified Penny.NonZero as NZ
import Penny.Polar
import qualified Penny.Positive as Pos
import Penny.Realm
import Penny.SeqUtil

import qualified Control.Lens as Lens
import Control.Monad ((>=>))
import Data.Semigroup (Semigroup((<>)))
import Data.Validation (AccValidation(AccFailure, AccSuccess))
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (Day, TimeOfDay)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pinchot (NonEmpty)
import qualified Pinchot

-- # Comments

comment
  :: Text
  -> AccValidation (NonEmpty Char) (Comment Char ())
comment = cComment

-- # Scalars

text
  :: Text
  -> Either (UnquotedString Char ()) (QuotedString Char ())
text = cString . Seq.fromList . X.unpack

day
  :: Day
  -> Maybe (Date Char ())
day = cDay

time
  :: TimeOfDay
  -> Maybe (Time Char ())
time = cTimeOfDay

integer
  :: Integer
  -> WholeAny Char ()
integer i = case NZ.c'NonZero'Integer i of
  Nothing -> WholeAny'Zero cZero
  Just nz -> WholeAny'WholeNonZero (WholeNonZero pm d1 (D0'9'Star ds))
    where
      pm | p == negative = PluMin'Opt (Just . PluMin'Minus $ cMinus)
         | otherwise = PluMin'Opt Nothing
        where
          p = Lens.view NZ.pole nz
      (d1, ds) = positiveDigits . NZ.c'Positive'NonZero $ nz

zone
  :: Int
  -> Maybe (Zone Char ())
zone i = do
  let (pm, i') | i >= 0 = (PluMin'Plus cPlus, i)
               | otherwise = (PluMin'Minus cMinus, negate i)
  (r, _) <- zoneDigit3 >=> zoneDigit2 >=> zoneDigit1 >=> zoneDigit0
    $ (\d3 d2 d1 d0 -> Zone cBacktick $ ZoneHrsMins pm d3 d2 d1 d0, i')
  return r
  where
    zoneDigit mk divisor (f, i) =
      let (d, m) = i `divMod` divisor
      in case mk d of
          Nothing -> Nothing
          Just dig -> Just (f dig, m)
    zoneDigit3 = zoneDigit c'D0'2'Int 1000
    zoneDigit2 = zoneDigit c'D0'3'Int 100
    zoneDigit1 = zoneDigit c'D0'9'Int 10
    zoneDigit0 = zoneDigit c'D0'9'Int 1

data ScalarError
  = InvalidDay Day
  | InvalidTime TimeOfDay
  | InvalidZone Int

scalar
  :: Scalar.Scalar
  -> Either ScalarError (Scalar Char ())
scalar sc = case sc of
  Scalar.SText txt -> Right $ case text txt of
    Left us -> Scalar'UnquotedString us
    Right qs -> Scalar'QuotedString qs
  Scalar.SDay dy -> case day dy of
    Nothing -> Left $ InvalidDay dy
    Just dt -> Right $ Scalar'Date dt
  Scalar.STime tod -> case time tod of
    Nothing -> Left $ InvalidTime tod
    Just t -> Right $ Scalar'Time t
  Scalar.SZone i -> case zone i of
    Nothing -> Left $ InvalidZone i
    Just z -> Right $ Scalar'Zone z
  Scalar.SInteger i -> Right . Scalar'WholeAny . integer $ i

-- # Trees


-- | Trees are not frozen if they are in the System realm.
-- Also, they are not frozen if they have no scalar and have no
-- children.
tree
  :: Tree.Tree
  -> AccValidation (NonEmpty ScalarError) (Maybe (Tree Char ()))
tree (Tree.Tree r s cs)
  | r == System = AccSuccess Nothing
  | otherwise = case s of
      Nothing -> case forest cs of
        AccFailure e -> AccFailure e
        AccSuccess mayForest -> case mayForest of
          Nothing -> AccSuccess Nothing
          Just forest -> AccSuccess . Just . Tree'ForestMaybeScalar
            $ ForestMaybeScalar
            (BracketedForest cOpenSquare mempty forest mempty cCloseSquare)
            (WhitesScalar'Opt Nothing)

      Just sc -> f <$> toAcc (scalar sc) <*> forest cs
        where
          toAcc = either (AccFailure . Pinchot.singleton) AccSuccess
          f scalar mayForest = case mayForest of
            Nothing -> Just . Tree'ScalarMaybeForest
              $ ScalarMaybeForest scalar (WhitesBracketedForest'Opt Nothing)
            Just forest ->
              Just . Tree'ScalarMaybeForest
              $ ScalarMaybeForest scalar
                  (WhitesBracketedForest'Opt
                    (Just (WhitesBracketedForest mempty
                      (BracketedForest cOpenSquare
                        mempty forest mempty cCloseSquare))))


forest
  :: Seq Tree.Tree
  -> AccValidation (NonEmpty ScalarError) (Maybe (Forest Char ()))
forest ts = f <$> traverse tree ts
  where
    f sq = case Pinchot.seqToNonEmpty (catMaybes sq) of
      Nothing -> Nothing
      Just (Pinchot.NonEmpty t1' ts') -> Just (Forest t1' next)
        where
          next = NextTree'Star (fmap f ts')
            where
              f t = NextTree mempty cComma mempty t

-- # Amounts

data AmountError = AmountError

-- | Amounts are always frozen as an ungrouped representation with
-- the commodity on the left with no space between.
amount
  :: Amount
  -> Either AmountError (Trio Char ())
amount = undefined

-- # Prices
data PriceError = PriceError

price
  :: PriceParts a
  -> Either PriceError (Price Char ())
price = undefined

-- # Transactions

data Tracompri
  = Tracompri'Transaction (Seq Tree.Tree) (Seq (Seq Tree.Tree, Amount))
  | Tracompri'Comment Text
  | Tracompri'Price (PriceParts ())

data WholeFileError = WholeFileError

wholeFile
  :: Seq Tracompri
  -> Either WholeFileError (WholeFile Char ())
wholeFile = undefined
