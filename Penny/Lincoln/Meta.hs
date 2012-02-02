module Penny.Lincoln.Meta where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Family.Family as F

import qualified Data.Map as M
import qualified Data.Text as X

data Side = CommodityOnLeft | CommodityOnRight deriving Show
data SpaceBetween = SpaceBetween | NoSpaceBetween deriving Show

data Format =
  Format { commodity :: B.Commodity
         , side :: Side
         , between :: SpaceBetween }
  deriving Show

newtype Line = Line { unLine :: Int }
               deriving Show

newtype Filename = Filename { unFilename :: X.Text }
                   deriving Show

class HasMemoLine a where
  memoLine :: a -> Maybe Line

class HasFilename a where
  filename :: a -> Filename

class MayHaveFormat a where
  maybeFormat :: a -> Maybe Format

class HasFormat a where
  format :: a -> Format

class HasMainLine a where
  line :: a -> Line

newtype TransactionMeta a b =
  TransactionMeta { unTransactionMeta :: F.Family a b }
  deriving Show
