module Penny.Lincoln.HasText where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as X

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty (TextNonEmpty, toText)

class HasText a where
  text :: a -> Text

instance HasText Text where
  text = id

instance HasText TextNonEmpty where
  text = toText

instance HasText B.SubAccountName where
  text = text . B.unSubAccountName

instance HasText B.SubCommodity where
  text = text . B.unSubCommodity

instance HasText B.Flag where
  text = text . B.unFlag

instance HasText B.MemoLine where
  text = text . B.unMemoLine

instance HasText B.Number where
  text = text . B.unNumber

instance HasText B.Payee where
  text = text . B.unPayee

instance HasText B.Tag where
  text = text . B.unTag
  
-- | Applying 'text' to a Delimited type will give you a single Text
-- with the delimiter interspersed between the values of the list.
data Delimited a = Delimited Text [a]
                 deriving Show

instance HasText a => HasText (Delimited a) where
  text (Delimited sep ts) = X.concat . intersperse sep . map text $ ts

class HasTextList a where
  textList :: a -> [Text]

instance HasText a => HasTextList (NonEmpty a) where
  textList = map text . toList

instance HasTextList B.Account where
  textList = textList . B.unAccount

instance HasTextList B.Commodity where
  textList = textList . B.unCommodity

instance HasTextList B.Tags where
  textList = map text . B.unTags

instance HasTextList B.Memo where
  textList = map text . B.unMemo

instance HasText a => HasTextList [a] where
  textList = map text

class HasTextNonEmpty a where
  textNonEmpty :: a -> TextNonEmpty

instance HasTextNonEmpty TextNonEmpty where
  textNonEmpty = id

instance HasTextNonEmpty B.SubAccountName where
  textNonEmpty = B.unSubAccountName

instance HasTextNonEmpty B.SubCommodity where
  textNonEmpty = B.unSubCommodity

instance HasTextNonEmpty B.Flag where
  textNonEmpty = B.unFlag
  
instance HasTextNonEmpty B.Number where
  textNonEmpty = B.unNumber

instance HasTextNonEmpty B.Payee where
  textNonEmpty = B.unPayee

instance HasTextNonEmpty B.Tag where
  textNonEmpty = B.unTag

class HasTextNonEmptyList a where
  textNonEmptyList :: a -> NonEmpty TextNonEmpty

instance HasTextNonEmpty a => HasTextNonEmptyList (NonEmpty a) where
  textNonEmptyList = fmap textNonEmpty

instance HasTextNonEmptyList B.Account where
  textNonEmptyList = fmap textNonEmpty . B.unAccount

instance HasTextNonEmptyList B.Commodity where
  textNonEmptyList = fmap textNonEmpty . B.unCommodity
