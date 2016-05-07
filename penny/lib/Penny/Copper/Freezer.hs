module Penny.Copper.Freezer where

import Penny.Amount
import Penny.Copper.Copperize
import Penny.Copper.PriceParts
import Penny.Copper.Types
import qualified Penny.Tree as Tree
import qualified Penny.Scalar as Scalar

import Data.Validation (AccValidation)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (Day, TimeOfDay)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pinchot (NonEmpty)

-- # Comments

data CommentError = BadCommentChar Char

comment
  :: Text
  -> Either CommentError (Comment Char ())
comment = either (Left . BadCommentChar) Right . cComment

-- # Scalars

text
  :: Text
  -> Either (UnquotedString Char ()) (QuotedString Char ())
text = cString . Seq.fromList . X.unpack

data DayError = DayError

day
  :: Day
  -> Either DayError (Date Char ())
day = undefined

data TimeError = TimeError

time
  :: TimeOfDay
  -> Either TimeError (Time Char ())
time = undefined

integer
  :: Integer
  -> WholeAny Char ()
integer = undefined

data ZoneError = ZoneError

zone
  :: Int
  -> Either ZoneError (Zone Char ())
zone = undefined

data ScalarError = ScalarError

scalar
  :: Scalar.Scalar
  -> Either ScalarError (Scalar Char ())
scalar = undefined

-- # Trees

data TreeError = TreeError

-- | Trees are not frozen if they are in the System realm.
tree
  :: Tree.Tree
  -> Either TreeError (Maybe (Tree Char ()))
tree = undefined

forest
  :: Seq Tree.Tree
  -> AccValidation (NonEmpty TreeError) (Forest Char ())
forest = undefined

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
