-- | Cabin color schemes
--
-- Each element of a Cabin report identifies what it is--a debit on an
-- even line, a credit on an odd line, etc. The user can have several
-- color schemes; the scheme contains color assignments for 8 and 256
-- color terminals. This allows the use of different schemes for light
-- and dark terminals or for any other reason.

module Penny.Cabin.Scheme where

import qualified Penny.Cabin.Chunk as C
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
  }

type Scheme = Labels (EvenAndOdd C.TextSpec)

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

makeChunk :: Scheme -> PreChunk -> C.Chunk
makeChunk s p =
  C.chunk (getEvenOddLabelValue (label p) (evenOdd p) s)
          (text p)
