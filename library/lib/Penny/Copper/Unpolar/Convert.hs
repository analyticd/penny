-- | Convert the parse tree to an Unpolar.

module Penny.Copper.Unpolar.Convert where

import Penny.Copper.Unpolar.Tree
import Penny.Numbers.Abstract.Unpolar hiding (Zeroes)
import qualified Penny.Numbers.Abstract.Unpolar as U
import Penny.Numbers.Abstract.Aggregates
import Data.Sums
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.RadGroup
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as S
import qualified Penny.Numbers.Natural as N

toNumbersZeroes :: Zeroes -> U.Zeroes
toNumbersZeroes (Zeroes _ sq) =
  U.Zeroes $ case N.nonNegToPos . N.length $ sq of
    Nothing -> N.onePos
    Just ps -> N.nextPos ps

nDF1RadixDigitsFromNDF1Radix
  :: NovDecs
  -> Radix r
  -> DecDecs
  -> NDF1RadixDigits r
  -> Unpolar r
nDF1RadixDigitsFromNDF1Radix nd rad dd ndf = case ndf of
  NDF1RadixDigitsEnd ->
    Unpolar . S2a . UngroupedUnpolar . S2b . UngroupedNonZero
    . S3b $ UNWholeRadix nd rad (Just dd)

  NDF1RadixDigitsGroups g1 gs ->
    Unpolar . S2b . GroupedUnpolar . S2b . GroupedNonZero . S6c
    $ MasunoGroupedRight nd rad dd g1 gs

nDF1RadixFromNDF1
  :: NovDecs
  -> Radix r
  -> NDF1Radix r
  -> Unpolar r
nDF1RadixFromNDF1 nd rad ndf = case ndf of
  NDF1RadixEnd ->
    Unpolar . S2a . UngroupedUnpolar . S2b . UngroupedNonZero
    . S3b $ UNWholeRadix nd rad Nothing
  NDF1RadixDigits dd rd -> nDF1RadixDigitsFromNDF1Radix nd rad dd rd

nDF1Group
  :: NovDecs
  -> Group r DecDecs
  -> Seq (Group r DecDecs)
  -> NDF1Group r
  -> Unpolar r
nDF1Group nd g1 gs x = case x of
  NDF1GroupEnd ->
    Unpolar . S2b . GroupedUnpolar . S2b . GroupedNonZero
    . S6a $ MasunoGroupedLeft nd g1 gs
  NDF1GroupRadix rd ndfr -> nDF1RadixFromNDF1Group nd g1 gs rd ndfr

nDF1RadixFromNDF1Group
  :: NovDecs
  -> Group r DecDecs
  -> Seq (Group r DecDecs)
  -> Radix r
  -> NDF1Radix r
  -> Unpolar r
nDF1RadixFromNDF1Group nd g1 gs rd ndf1 = case ndf1 of
  NDF1RadixEnd -> Unpolar . S2b . GroupedUnpolar . S2b . GroupedNonZero
    . S6b $ MasunoGroupedLeftRad mgl rd Nothing
  NDF1RadixDigits dd ndfRd ->
    nDF1RadixDigitsFromNDF1Group nd g1 gs rd dd ndfRd
  where
    mgl = MasunoGroupedLeft nd g1 gs

nDF1RadixDigitsFromNDF1Group
  :: NovDecs
  -> Group r DecDecs
  -> Seq (Group r DecDecs)
  -> Radix r
  -> DecDecs
  -> NDF1RadixDigits r
  -> Unpolar r
nDF1RadixDigitsFromNDF1Group nd g1 gs rd dd ndf =
  Unpolar . S2b . GroupedUnpolar
  . S2b . GroupedNonZero . S6b
  $ MasunoGroupedLeftRad mgl rd (Just (dd, sq))
  where
    mgl = MasunoGroupedLeft nd g1 gs
    sq = case ndf of
      NDF1RadixDigitsEnd -> S.empty
      NDF1RadixDigitsGroups gr1 grs -> (gr1 <| grs)

nDF1
  :: NovDecs
  -> NDF1 r
  -> Unpolar r
nDF1 nd x = case x of
  NDF1End -> Unpolar . S2a . UngroupedUnpolar . S2b .
    UngroupedNonZero . S3a $ UNWhole nd
  NDF1Radix r ndfr -> nDF1RadixFromNDF1 nd r ndfr
  NDF1Group g1 gs ndfg -> nDF1Group nd g1 gs ndfg

novDecsFirst
  :: NovDecsFirst r
  -> Unpolar r
novDecsFirst (NovDecsFirst nd ndf1) = nDF1 nd ndf1

aRZNext
  :: HasZeroDigit
  -> Radix r
  -> Zeroes
  -> Group r Zeroes
  -> Seq (Group r Zeroes)
  -> ARZNext r
  -> Unpolar r
aRZNext hzd rdx zs g1 gs x = Unpolar . S2b . GroupedUnpolar $ case x of
  ARZNEnd -> S2a $ GZ hzd rdx zs' g1' gs'
  ARZNNovDecs nd dd -> S2b . GroupedNonZero . S6f
    $ FracunoFirstGroupZNovDecs hzd rdx zs' g1' gs' nd dd
  ARZNGroups gmz sq -> S2b . GroupedNonZero . S6d
    $ FracunoFirstGroupZ hzd rdx zs' (g1' <| gs') (fmap convGmz gmz) sq
  where
    zs' = toNumbersZeroes zs
    g1' = fmap toNumbersZeroes g1
    gs' = fmap (fmap toNumbersZeroes) gs
    convGmz (mz, nd) = ZeroesNovDecs zz nd
      where
        zz = maybe N.nonNegZero
          (N.nonNegToPos . U.unZeroes . toNumbersZeroes) mz
