module Penny.Numbers.Unsigned
  ( Unsigned
  , unsigned
  , unUnsigned
  , add
  , mult
  , pow
  , ten
  , monus
  , allocate
  , zero
  , novDecsToUnsigned
  , unsignedToNovDecs
  ) where

import Penny.Numbers.Concrete hiding (zero)
import Deka.Native.Abstract
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Sequence (Seq, ViewL(..), (<|), (|>))
import Data.Ord

newtype Unsigned = Unsigned { unUnsigned :: Integer }
  deriving (Eq, Ord, Show)

unsigned :: Integer -> Maybe Unsigned
unsigned i
  | i < 0 = Nothing
  | otherwise = Just $ Unsigned i

add :: Unsigned -> Unsigned -> Unsigned
add (Unsigned x) (Unsigned y) = Unsigned $ x + y

mult :: Unsigned -> Unsigned -> Unsigned
mult (Unsigned x) (Unsigned y) = Unsigned $ x * y

pow :: Unsigned -> Unsigned -> Unsigned
pow (Unsigned x) (Unsigned y) = Unsigned $ x ^ y

ten :: Unsigned
ten = Unsigned 10

-- | Subtracts the second operand from the first, or zero if the
-- second operand is bigger.
monus :: Unsigned -> Unsigned -> Unsigned
monus (Unsigned x) (Unsigned y) = Unsigned $ max 0 (x - y)

-- | Allocate unsigned numbers.
allocate
  :: Unsigned
  -- ^ The sum of the allocations will always add up to this sum.
  -> Seq Unsigned
  -- ^ Allocate these integers
  -> Seq Unsigned
  -- ^ The resulting allocations.  The sum will alwas add up to the
  -- given sum, unless the imput sequence is empty, in which case this
  -- sequence will also be empty.
allocate tot ls
  | S.null ls = S.empty
  | otherwise
      = fmap snd
      . S.sortBy (comparing fst)
      . allocFinal tot
      . S.sortBy cmp
      . allocInitial tot
      . S.zip (S.iterateN (S.length ls) succ 0)
      $ ls
  where
    cmp (_, _, x) (_, _, y) = y `compare` x

-- | Perform initial allocations based on integer quotient.
allocInitial
  :: Unsigned
  -- ^ Total to allocate
  -> Seq (Int, Unsigned)
  -- ^ Index of each number to allocate, and the number itself.
  -> Seq (Int, Unsigned, Double)
  -- ^ Index, the initial assignment, and the remainder
allocInitial tot sq = fmap assign sq
  where
    assign (ix, (Unsigned votes)) = (ix, Unsigned seats, rmdr)
      where
        (seats, rmdr) = properFraction $
          fromIntegral votes / quota
    totVotes = fromIntegral . F.sum . fmap (unUnsigned . snd) $ sq
    quota = totVotes / fromIntegral (unUnsigned tot)


-- | Make final allocations based on remainders.
allocFinal
  :: Unsigned
  -- ^ Total to allocate
  -> Seq (Int, Unsigned, Double)
  -- ^ Index, initial assignment, and remainder
  -> Seq (Int, Unsigned)
  -- ^ Index, and final allocations
allocFinal (Unsigned tot) sq = go rmdr sq
  where

    rmdr
      | diff < 0 = error "allocFinal: error: too much allocated"
      | otherwise = diff
      where
        diff = tot - already
        already = F.sum . fmap (\(_, Unsigned x, _) -> x) $ sq

    go leftOver ls = case S.viewl ls of
      EmptyL -> S.empty
      (ix, (Unsigned intl), _) :< rest ->
        (ix, Unsigned this) <| go leftOver' rest
        where
          (this, leftOver')
            | leftOver > 0 = (succ intl, pred leftOver)
            | otherwise = (intl, leftOver)

zero :: Unsigned
zero = Unsigned 0

novDecsToUnsigned :: NovDecs -> Unsigned
novDecsToUnsigned = Unsigned . novDecsToInt

unsignedToNovDecs :: Unsigned -> Maybe NovDecs
unsignedToNovDecs (Unsigned start)
  | start == 0 = Nothing
  | otherwise = Just . finish $ go start
  where
    finish sq = case S.viewl sq of
      EmptyL -> error "unsignedToNovDecs: empty sequence"
      x :< xs -> case x of
        Nonem n -> NovDecs n xs
        _ -> error "unsignedToNovDecs: leading zero"

    go i = rest |> this
      where
        rest | qt == 0 = S.empty
             | otherwise = go qt
        (qt, rm) = i `divMod` 10
        this = case intToDecem rm of
          Just dc -> dc
          Nothing -> error "unsignedToNovDecs: bad remainder"
