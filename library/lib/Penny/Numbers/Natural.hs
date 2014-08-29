{-# LANGUAGE BangPatterns #-}
module Penny.Numbers.Natural
  ( NE(..)
  , flatten
  , Pos
  , unPos
  , pos
  , addPos
  , multPos
  , prevPos
  , nextPos
  , novemToPos
  , posToNovem

  , NonNeg
  , unNonNeg
  , nonNeg
  , posToNonNeg
  , nonNegToPos
  , addNonNeg
  , multNonNeg
  , monusNonNeg
  , subtPos
  , expPos
  , nextNonNeg
  , subtPosFromNonNeg
  , divNonNegByPos
  , expNonNeg
  , decemToNonNeg
  , nonNegToDecem

  , novDecsToNonNeg
  , novDecsToInt
  , nonNegToNovDecs
  , posToNovDecs
  , novDecsToPos

  , nonNegZero
  , tenNonNeg
  , tenPos
  , zeroNonNeg
  , onePos

  , length
  , lengthNE
  , allocate
  ) where

import Data.Maybe
import Data.Ord (comparing)
import Deka.Native.Abstract
import qualified Data.Foldable as F
import Data.Sequence (Seq, ViewL(..), (<|), (|>), ViewR(..))
import qualified Data.Sequence as S
import Prelude hiding (length)
import qualified Prelude

-- | A non-empty set that starts with something of one type and
-- concludes with a list of items of a different type.
data NE a b = NE a (Seq b)
  deriving (Eq, Ord, Show)

flatten :: NE a a -> Seq a
flatten (NE a b) = a <| b

lengthNE :: NE a b -> Pos
lengthNE (NE _ sq) = Pos . succ . fromIntegral . S.length $ sq

data Pos = Pos { unPos :: Integer }
  deriving (Eq, Ord, Show)

pos :: Integer -> Maybe Pos
pos i
  | i < 1 = Nothing
  | otherwise = Just $ Pos i

addPos :: Pos -> Pos -> Pos
addPos (Pos x) (Pos y) = Pos $ x + y

multPos :: Pos -> Pos -> Pos
multPos (Pos x) (Pos y) = Pos $ x * y

prevPos :: Pos -> Maybe Pos
prevPos (Pos i)
  | i == 1 = Nothing
  | otherwise = Just . Pos $ pred i

data NonNeg = NonNeg { unNonNeg :: Integer }
  deriving (Eq, Ord, Show)

posToNonNeg :: Pos -> NonNeg
posToNonNeg (Pos i) = NonNeg i

nonNegToPos :: NonNeg -> Maybe Pos
nonNegToPos (NonNeg i)
  | i < 1 = Nothing
  | otherwise = Just $ Pos i

nonNeg :: Integer -> Maybe NonNeg
nonNeg i
  | i < 0 = Nothing
  | otherwise = Just $ NonNeg i

addNonNeg :: NonNeg -> NonNeg -> NonNeg
addNonNeg (NonNeg x) (NonNeg y) = NonNeg $ x + y

multNonNeg :: NonNeg -> NonNeg -> NonNeg
multNonNeg (NonNeg x) (NonNeg y) = NonNeg $ x + y

monusNonNeg :: NonNeg -> NonNeg -> NonNeg
monusNonNeg (NonNeg x) (NonNeg y) = NonNeg . max 0 $ x - y

subtPos :: Pos -> Pos -> Maybe NonNeg
subtPos (Pos x) (Pos y)
  | y > x = Nothing
  | otherwise = Just $ NonNeg (x - y)

expPos :: Pos -> Pos -> Pos
expPos (Pos x) (Pos y) = Pos $ x ^ y

novemToPos :: Novem -> Pos
novemToPos = Pos . novemToInt

posToNovem :: Pos -> Maybe Novem
posToNovem (Pos p)
  | p == 1 = Just D1
  | p == 2 = Just D2
  | p == 3 = Just D3
  | p == 4 = Just D4
  | p == 5 = Just D5
  | p == 6 = Just D6
  | p == 7 = Just D7
  | p == 8 = Just D8
  | p == 9 = Just D9
  | otherwise = Nothing

nextNonNeg :: NonNeg -> NonNeg
nextNonNeg (NonNeg x) = NonNeg $ succ x

nextPos :: Pos -> Pos
nextPos (Pos x) = Pos $ succ x

subtPosFromNonNeg :: NonNeg -> Pos -> Maybe NonNeg
subtPosFromNonNeg (NonNeg x) (Pos y)
  | y > x = Nothing
  | otherwise = Just . NonNeg $ x - y

divNonNegByPos :: NonNeg -> Pos -> (NonNeg, NonNeg)
divNonNegByPos (NonNeg n) (Pos d) = (NonNeg q, NonNeg r)
  where
    (q, r) = n `divMod` d

expNonNeg :: NonNeg -> NonNeg -> NonNeg
expNonNeg (NonNeg x) (NonNeg y) = NonNeg $ x ^ y

decemToNonNeg :: Decem -> NonNeg
decemToNonNeg d = case d of
  D0 -> NonNeg 0
  Nonem x -> NonNeg n
    where
      Pos n = novemToPos x

nonNegToDecem :: NonNeg -> Maybe Decem
nonNegToDecem (NonNeg n)
  | n == 0 = Just D0
  | otherwise = fmap Nonem $ posToNovem (Pos n)

nonNegZero :: NonNeg
nonNegZero = NonNeg 0

tenNonNeg :: NonNeg
tenNonNeg = NonNeg 10

tenPos :: Pos
tenPos = Pos 10

zeroNonNeg :: NonNeg
zeroNonNeg = NonNeg 0

onePos :: Pos
onePos = Pos 1

length :: F.Foldable f => f a -> NonNeg
length = NonNeg . fromIntegral . Prelude.length . F.toList

novDecsToNonNeg :: NE Novem Decem -> NonNeg
novDecsToNonNeg = NonNeg . novDecsToInt

novDecsToInt :: Integral a => NE Novem Decem -> a
novDecsToInt (NE n ds) = finish $ go 0 (0 :: Int) ds
  where
    go !acc !plcs sq = case S.viewr sq of
      EmptyR -> (acc, plcs)
      xs :> x ->
        go (acc + decemToInt x * 10 ^ plcs) (succ plcs) xs
    finish (acc, plcs) = acc + novemToInt n * 10 ^ plcs

-- | Allocate unsigned numbers.
allocate
  :: NonNeg
  -- ^ The sum of the allocations will always add up to this sum.
  -> Seq NonNeg
  -- ^ Allocate these integers
  -> Seq NonNeg
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
  :: NonNeg
  -- ^ Total to allocate
  -> Seq (Int, NonNeg)
  -- ^ Index of each number to allocate, and the number itself.
  -> Seq (Int, NonNeg, Double)
  -- ^ Index, the initial assignment, and the remainder
allocInitial tot sq = fmap assign sq
  where
    assign (ix, (NonNeg votes)) = (ix, NonNeg seats, rmdr)
      where
        (seats, rmdr) = properFraction $
          fromIntegral votes / quota
    totVotes = fromIntegral . F.sum . fmap (unNonNeg . snd) $ sq
    quota = totVotes / fromIntegral (unNonNeg tot)


-- | Make final allocations based on remainders.
allocFinal
  :: NonNeg
  -- ^ Total to allocate
  -> Seq (Int, NonNeg, Double)
  -- ^ Index, initial assignment, and remainder
  -> Seq (Int, NonNeg)
  -- ^ Index, and final allocations
allocFinal (NonNeg tot) sq = go rmdr sq
  where

    rmdr
      | diff < 0 = error "allocFinal: error: too much allocated"
      | otherwise = diff
      where
        diff = tot - already
        already = F.sum . fmap (\(_, NonNeg x, _) -> x) $ sq

    go leftOver ls = case S.viewl ls of
      EmptyL -> S.empty
      (ix, (NonNeg intl), _) :< rest ->
        (ix, NonNeg this) <| go leftOver' rest
        where
          (this, leftOver')
            | leftOver > 0 = (succ intl, pred leftOver)
            | otherwise = (intl, leftOver)

nonNegToNovDecs :: NonNeg -> Maybe (NE Novem Decem)
nonNegToNovDecs (NonNeg start)
  | start == 0 = Nothing
  | otherwise = Just . finish $ go start
  where
    finish sq = case S.viewl sq of
      EmptyL -> error "nonNegToNovDecs: empty sequence"
      x :< xs -> case x of
        Nonem n -> NE n xs
        _ -> error "nonNegToNovDecs: leading zero"

    go i = rest |> this
      where
        rest | qt == 0 = S.empty
             | otherwise = go qt
        (qt, rm) = i `divMod` 10
        this = case intToDecem rm of
          Just dc -> dc
          Nothing -> error "nonNegToNovDecs: bad remainder"

posToNovDecs :: Pos -> NE Novem Decem
posToNovDecs = finish . S.unfoldl unfolder . posToNonNeg
  where
    unfolder nn
      | qt == nonNegZero = Nothing
      | otherwise = Just (qt, dg)
      where
        (qt, rm) = divNonNegByPos nn tenPos
        dg = fromMaybe (error "posToNovDecs: error: digit greater than 9")
          . nonNegToDecem $ rm

    finish acc = case S.viewl acc of
      EmptyL -> error "posToNovDecs: error: empty accumulator"
      beg :< rest -> case beg of
        D0 -> error "posToNovDecs: error: zero first digit"
        Nonem n -> NE n rest

novDecsToPos :: NE Novem Decem -> Pos
novDecsToPos (NE nv ds) = finish $ go zeroNonNeg zeroNonNeg ds
  where

    go acc places sq = case S.viewr sq of
      EmptyR -> (acc, places)
      rest :> dig -> go acc' (nextNonNeg places) rest
        where
          acc' = addNonNeg acc
               . multNonNeg (decemToNonNeg dig)
               . expNonNeg tenNonNeg
               $ places

    finish (NonNeg acc, NonNeg places) = Pos $ acc + n * 10 ^ places
      where
        Pos n = novemToPos nv
