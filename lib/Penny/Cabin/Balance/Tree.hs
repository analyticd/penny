-- | Takes postings and places them into a tree for further
-- processing.
--
-- Steps:
--
-- * 1. [LT.PostingInfo] -> [PriceConverted]
--
-- * 2. [PriceConverted] -> FlatMap
--
-- * 3. FlatMap -> RawBals
--
-- * 4. RawBals -> (SummedBals, TotalBal)
--
-- * 5. (SummedBals, TotalBal) -> [K.Row]
--
-- * 6. [K.Row] -> [Chunk.Chunk]
--
-- This module provides the functions necessary to generate the
-- Balance report, after the options have either been parsed from the
-- command line or if the options have already been supplied in the
-- program (which can be useful if you have a program which is
-- printing a Balance report.)
--
-- To make a Balance report, you must first convert the list of Boxes
-- with LibertyMeta to PriceConverteds; then you can generate the
-- report.
module Penny.Cabin.Balance.Tree (
  report
  , TreeOpts(..)
  , PriceConverteds
  , nullConvert
  , converter
  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Penny.Lincoln.NestedMap as NM
import qualified Data.Text as X
import qualified Data.Tree as E
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Balance.Chunker as K
import qualified Penny.Cabin.Balance.Util as U
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Balance as Bal
import qualified Data.Semigroup as S

-- Step 1. Convert prices.

data PriceConverted = PriceConverted {
  pcEntry :: L.Entry
  , pcAccount :: L.Account }

newtype PriceConverteds =
  PriceConverteds { unPriceConverteds :: [PriceConverted] }

convertOne ::
  L.PriceDb
  -> (L.Commodity, L.DateTime)
  -> L.Entry
  -> Ex.Exceptional X.Text L.Entry
convertOne db (cty, dt) en@(L.Entry dc am@(L.Amount _ fr)) =
  if fr == cty
  then return en
  else do
    let to = L.To cty
    qt' <- case L.convert db dt to am of
      Ex.Exception e -> Ex.throw (convertError cty am e)
      Ex.Success g -> return g
    return (L.Entry dc (L.Amount qt' cty))

convertError ::
  L.Commodity
  -> L.Amount
  -> L.PriceDbError
  -> X.Text
convertError to (L.Amount _ fr) e =
  let fromErr = L.text (L.Delimited (X.singleton ':')
                        (Fdbl.toList . L.unCommodity $ fr))
      toErr = L.text (L.Delimited (X.singleton ':')
                      (Fdbl.toList . L.unCommodity $ to))
  in case e of
    L.FromNotFound ->
      X.pack "no data to convert from commodity "
      `X.append` fromErr
    L.ToNotFound ->
      X.pack "no data to convert to commodity "
      `X.append` toErr
    L.CpuNotFound ->
      X.pack "no data to convert from commodity "
      `X.append` fromErr
      `X.append` (X.pack " to commodity ")
      `X.append` toErr
      `X.append` (X.pack " at given date and time")
  

buildDb :: [L.PricePoint] -> L.PriceDb
buildDb = foldl f L.emptyDb where
  f db pb = L.addPrice db pb

-- | Makes PriceConverteds where there is no target commodity. Always
-- succeeds.
nullConvert :: [L.Box Ly.LibertyMeta] -> PriceConverteds
nullConvert =
  let f b = PriceConverted en ac
        where
          p = L.boxPostFam b
          en = Q.entry p
          ac = Q.account p
    in PriceConverteds . map f

-- | Makes PriceConverteds where commodities will have to be changed
-- to a target commodity. This will fail if necessary price data is
-- lacking.
converter ::
  (L.Commodity, L.DateTime)
  -- ^ Convert to this commodity, using the price data as of this date and time

  -> [L.PricePoint]
  -- ^ Price data to use

  -> [L.Box a]
  -- ^ The postings

  -> Ex.Exceptional X.Text PriceConverteds
  -- ^ If a posting fails to convert because there is not price data,
  -- an exception is returned with an explanatory Text. Otherwise, the
  -- converted prices are returned.

converter conv pbs bs =
    let f b = let p = L.boxPostFam b
                  en = Q.entry p
                  ac = Q.account p
                 in do
                   en' <- convertOne db conv en
                   return (PriceConverted en' ac)
        db = buildDb pbs
    in fmap PriceConverteds $ mapM f bs


-- Step 2. This puts all the PriceConverteds into a flat map, where
-- the key is the full account name and the value is the balance. If
-- the user desired, zero balances are eliminated from the flat map.
newtype FlatMap = FlatMap { _unFlatMap :: M.Map L.Account Bal.Balance }

toFlatMap :: TreeOpts -> PriceConverteds -> FlatMap
toFlatMap o = FlatMap . foldr f M.empty . unPriceConverteds where
  remove = not . CO.unShowZeroBalances . showZeroBalances $ o
  f pc m =
    let a = pcAccount pc
        bal = Bal.entryToBalance . pcEntry $ pc
    in case M.lookup a m of
      Nothing -> M.insert a bal m
      Just oldBal ->
        let added = Bal.addBalances oldBal bal
            newBal = if remove
                     then Bal.removeZeroCommodities added
                     else added
        in if M.null . Bal.unBalance $ newBal
           then M.delete a m
           else M.insert a newBal m

-- Step 3
newtype RawBal = RawBal { unRawBal :: Bal.Balance }
instance Monoid.Monoid RawBal where
  mappend (RawBal b1) (RawBal b2) = RawBal $ b1 `Monoid.mappend` b2
  mempty = RawBal Monoid.mempty

type RawBals = NM.NestedMap L.SubAccountName RawBal

-- | Inserts a pair from the FlatMap into the Balances NestedMap.
insertBalance ::
  L.Account
  -> Bal.Balance
  -> RawBals
  -> RawBals
insertBalance a b rbs = let
  rb = RawBal b
  subs = Fdbl.toList . L.unAccount $ a
  in NM.insert rbs subs rb
  
rawBalances :: FlatMap -> RawBals
rawBalances (FlatMap m) = M.foldrWithKey insertBalance NM.empty m

-- Step 4
newtype SummedBal =
  SummedBal { unSummedBal :: Bal.Balance }
instance Monoid.Monoid SummedBal where
  mappend (SummedBal b1) (SummedBal b2) =
    SummedBal $ b1 `Monoid.mappend` b2
  mempty = SummedBal Monoid.mempty
type TotalBal = SummedBal
type SummedBals = NM.NestedMap L.SubAccountName SummedBal

sumBalances :: RawBals -> (SummedBals, TotalBal)
sumBalances rb = (sb, (SummedBal . unRawBal $ tb)) where
  (tb, rawBals) = NM.cumulativeTotal rb
  sb = fmap (SummedBal . unRawBal) rawBals

-- Step 5 (new)
makeRows :: (SummedBals, TotalBal) -> [K.Row]
makeRows (sb, tb) = first:rest
  where
    first = K.Row 0 (X.pack "Total") totBal
    totBal = M.assocs . L.unBalance . unSummedBal $ tb
    flat = concatMap E.flatten
           . map U.labelLevels
           . NM.toForest $ sb
    toRow (lvl, (sub, bal)) = K.Row lvl acctName balList
      where
        acctName = L.text sub
        balList = M.assocs . L.unBalance . unSummedBal $ bal
    rest = map toRow flat

-- Step 6 (New)
makeChunks ::
  TreeOpts
  -> [K.Row]
  -> [Chunk.Chunk]
makeChunks o rs =
  K.rowsToChunks (balanceFormat o) (drCrColors o)
  (baseColors o) rs

-- Options

-- | Options for making the balance report. These are the only options
-- needed to make the report if the options are not being parsed in
-- from the command line.
data TreeOpts = TreeOpts {
  drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , balanceFormat :: L.Commodity -> L.Qty -> X.Text
  , showZeroBalances :: CO.ShowZeroBalances
  }

-- Tie it all together

-- | Creates the Balance report.
report ::
  TreeOpts
  -> PriceConverteds
  -> [Chunk.Chunk]
report os  =
    makeChunks os
    . makeRows
    . sumBalances
    . rawBalances
    . toFlatMap os

