-- | Cabin color schemes
--
-- Each element of a Cabin report identifies what it is--a debit on an
-- even line, a credit on an odd line, etc. The user can have several
-- color schemes; the scheme contains color assignments for 8 and 256
-- color terminals. This allows the use of different schemes for light
-- and dark terminals or for any other reason.

module Penny.Cabin.Scheme where

import qualified Penny.Steel.Chunk as C
import qualified Penny.Cabin.Meta as M
import qualified Penny.Lincoln as L
import qualified Data.Text as X

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

type TextSpecs = Labels (EvenAndOdd C.TextSpec)

data Scheme = Scheme
  { name :: String
    -- ^ The name of this scheme. How it will be identified on the
    -- command line.

  , description :: String
    -- ^ A brief (one-line) description of what this scheme is, such
    -- as @for dark background terminals@

  , textSpecs :: TextSpecs
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

data PreChunk = PreChunk
  { label :: Label
  , evenOdd :: EvenOdd
  , text :: X.Text
  } deriving (Eq, Show)

width :: PreChunk -> C.Width
width = C.Width . X.length . text

makeChunk :: TextSpecs -> PreChunk -> C.Chunk
makeChunk s p =
  C.chunk (getEvenOddLabelValue (label p) (evenOdd p) s)
          (text p)

fromVisibleNum :: M.VisibleNum -> EvenOdd
fromVisibleNum vn =
  let s = M.unVisibleNum vn in
  if even . L.forward $ s then Even else Odd

dcToLbl :: L.DrCr -> Label
dcToLbl L.Debit = Debit
dcToLbl L.Credit = Credit

bottomLineToDrCr :: L.BottomLine -> EvenOdd -> PreChunk
bottomLineToDrCr bl eo = PreChunk lbl eo t
  where
    (lbl, t) = case bl of
      L.Zero -> (Zero, X.pack "--")
      L.NonZero (L.Column clmDrCr _) -> case clmDrCr of
        L.Debit -> (Debit, X.singleton '<')
        L.Credit -> (Credit, X.singleton '>')

balancesToCmdtys
  :: EvenOdd
  -> [(L.Commodity, L.BottomLine)]
  -> [PreChunk]
balancesToCmdtys eo ls =
  if null ls
  then [PreChunk Zero eo (X.pack "--")]
  else map (bottomLineToCmdty eo) ls

bottomLineToCmdty
  :: EvenOdd
  -> (L.Commodity, L.BottomLine)
  -> PreChunk
bottomLineToCmdty eo (cy, bl) = PreChunk lbl eo t
  where
    t = L.unCommodity cy
    lbl = case bl of
      L.Zero -> Zero
      L.NonZero (L.Column clmDrCr _) -> dcToLbl clmDrCr

balanceToQtys
  :: (L.Commodity -> L.Qty -> X.Text)
  -> EvenOdd
  -> [(L.Commodity, L.BottomLine)]
  -> [PreChunk]
balanceToQtys getTxt eo ls =
  if null ls
  then [PreChunk Zero eo (X.pack "--")]
  else map (bottomLineToQty getTxt eo) ls


bottomLineToQty
  :: (L.Commodity -> L.Qty -> X.Text)
  -> EvenOdd
  -> (L.Commodity, L.BottomLine)
  -> PreChunk
bottomLineToQty getTxt eo (cy, bl) = PreChunk lbl eo t
  where
    (lbl, t) = case bl of
      L.Zero -> (Zero, X.pack "--")
      L.NonZero (L.Column clmDrCr qt) -> (dcToLbl clmDrCr, getTxt cy qt)

