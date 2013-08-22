{-# LANGUAGE OverloadedStrings #-}
-- | Cabin color schemes
--
-- Each element of a Cabin report identifies what it is--a debit on an
-- even line, a credit on an odd line, etc. The user can have several
-- color schemes; the scheme contains color assignments for 8 and 256
-- color terminals. This allows the use of different schemes for light
-- and dark terminals or for any other reason.

module Penny.Cabin.Scheme where

import Data.Monoid (mempty)
import qualified Penny.Cabin.Meta as M
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified System.Console.Rainbow as R

data Label
  = Debit
  | Credit
  | Zero
  | Other
  deriving (Eq, Ord, Show)

data EvenOdd = Even | Odd deriving (Eq, Ord, Show)

data Labels a = Labels
  { debit :: a
  , credit :: a
  , zero :: a
  , other :: a
  } deriving Show

getLabelValue :: Label -> Labels a -> a
getLabelValue l ls = case l of
  Debit -> debit ls
  Credit -> credit ls
  Zero -> zero ls
  Other -> other ls

data EvenAndOdd a = EvenAndOdd
  { eoEven :: a
  , eoOdd :: a
  } deriving Show

type Changers = Labels (EvenAndOdd (R.Chunk -> R.Chunk))

data Scheme = Scheme
  { name :: String
    -- ^ The name of this scheme. How it will be identified on the
    -- command line.

  , description :: String
    -- ^ A brief (one-line) description of what this scheme is, such
    -- as @for dark background terminals@

  , changers :: Changers
  } deriving Show


getEvenOdd :: EvenOdd -> EvenAndOdd a -> a
getEvenOdd eo eao = case eo of
  Even -> eoEven eao
  Odd -> eoOdd eao

getEvenOddLabelValue
  :: Label
  -> EvenOdd
  -> Labels (EvenAndOdd a)
  -> a
getEvenOddLabelValue l eo ls =
  getEvenOdd eo (getLabelValue l ls)

fromVisibleNum :: M.VisibleNum -> EvenOdd
fromVisibleNum vn =
  let s = M.unVisibleNum vn in
  if even . L.forward $ s then Even else Odd

dcToLbl :: L.DrCr -> Label
dcToLbl L.Debit = Debit
dcToLbl L.Credit = Credit

bottomLineToDrCr :: L.BottomLine -> EvenOdd -> Changers -> R.Chunk
bottomLineToDrCr bl eo chgrs = md c
  where
    (c, md) = case bl of
      L.Zero -> ("--", getEvenOddLabelValue Zero eo chgrs)
      L.NonZero (L.Column clmDrCr _) -> case clmDrCr of
        L.Debit -> ("<", getEvenOddLabelValue Debit eo chgrs)
        L.Credit -> (">", getEvenOddLabelValue Credit eo chgrs)


balancesToCmdtys
  :: Changers
  -> EvenOdd
  -> [(L.Commodity, L.BottomLine)]
  -> [R.Chunk]
balancesToCmdtys chgrs eo ls =
  if null ls
  then [getEvenOddLabelValue Zero eo chgrs $ "--"]
  else map (bottomLineToCmdty chgrs eo) ls

bottomLineToCmdty
  :: Changers
  -> EvenOdd
  -> (L.Commodity, L.BottomLine)
  -> R.Chunk
bottomLineToCmdty chgrs eo (cy, bl) = md c
  where
    c = R.Chunk mempty . L.unCommodity $ cy
    lbl = case bl of
      L.Zero -> Zero
      L.NonZero (L.Column clmDrCr _) -> dcToLbl clmDrCr
    md = getEvenOddLabelValue lbl eo chgrs

balanceToQtys
  :: Changers
  -> (L.Amount L.Qty -> X.Text)
  -> EvenOdd
  -> [(L.Commodity, L.BottomLine)]
  -> [R.Chunk]
balanceToQtys chgrs getTxt eo ls =
  if null ls
  then let md = getEvenOddLabelValue Zero eo chgrs
       in [md "--"]
  else map (bottomLineToQty chgrs getTxt eo) ls


bottomLineToQty
  :: Changers
  -> (L.Amount L.Qty -> X.Text)
  -> EvenOdd
  -> (L.Commodity, L.BottomLine)
  -> R.Chunk
bottomLineToQty chgrs getTxt eo (cy, bl) = md (R.Chunk mempty t)
  where
    (lbl, t) = case bl of
      L.Zero -> (Zero, X.pack "--")
      L.NonZero (L.Column clmDrCr qt) ->
        (dcToLbl clmDrCr, getTxt (L.Amount qt cy))
    md = getEvenOddLabelValue lbl eo chgrs

