-- | Convert the parse tree to an Unpolar.

module Penny.Copper.Unpolar.Convert where

import Penny.Copper.Unpolar.Tree
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Abstract.Aggregates
import Data.Sums
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.RadGroup
import Data.Sequence (Seq)

nDF1RadixDigits
  :: NovDecs
  -> Radix r
  -> DecDecs
  -> NDF1RadixDigits r
  -> Unpolar r
nDF1RadixDigits nd rad dd ndf = case ndf of
  NDF1RadixDigitsEnd ->
    Unpolar . S2a . UngroupedUnpolar . S2b . UngroupedNonZero
    . S3b $ UNWholeRadix nd rad (Just dd)

  NDF1RadixDigitsGroups g1 gs ->
    Unpolar . S2b . GroupedUnpolar . S2b . GroupedNonZero . S5c
    $ MasunoGroupedRight nd rad dd g1 gs

-- | an 'NDF1Radix' occuring directly underneath an 'NDF1'.
nDF1RadixFromNDF1
  :: NovDecs
  -> Radix r
  -> NDF1Radix r
  -> Unpolar r
nDF1RadixFromNDF1 nd rad ndf = case ndf of
  NDF1RadixEnd ->
    Unpolar . S2a . UngroupedUnpolar . S2b . UngroupedNonZero
    . S3b $ UNWholeRadix nd rad Nothing
  NDF1RadixDigits dd rd -> nDF1RadixDigits nd rad dd rd

-- | an 'NDF1GroupRadix' occuring from an 'NDF1Group'.
nDF1GroupRadixFromNDF1Group
  :: NovDecs
  -> Group r DecDecs
  -> Seq (Group r DecDecs)
  -> Radix r
  -> NDF1Group r
  -> Unpolar r
nDF1GroupRadixFromNDF1Group nd g1 gs rd x = case x of
  NDF1GroupEnd ->
    Unpolar . S2b . GroupedUnpolar . S2b . GroupedNonZero
    . S5b $ MasunoGroupedLeftRad mgl rd Nothing
  NDF1GroupRa
  where
    mgl = MasunoGroupedLeft nd g1 gs

nDF1Group
  :: NovDecs
  -> Group r DecDecs
  -> Seq (Group r DecDecs)
  -> NDF1Group r
  -> Unpolar r
nDF1Group nd g1 gs x = case x of
  NDF1GroupEnd ->
    Unpolar . S2b . GroupedUnpolar . S2b . GroupedNonZero
    . S5a $ MasunoGroupedLeft nd g1 gs
  NDF1GroupRadix rd ndfr -> undefined

nDF1
  :: NovDecs
  -> NDF1 r
  -> Unpolar r
nDF1 nd x = case x of
  NDF1End -> Unpolar . S2a . UngroupedUnpolar . S2b .
    UngroupedNonZero . S3a $ UNWhole nd
  NDF1Radix r ndfr -> nDF1RadixFromNDF1 nd r ndfr
  _ -> undefined
