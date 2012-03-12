module Penny.Lincoln.HasText where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, cons)
import qualified Data.Text as X

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty (TextNonEmpty(TextNonEmpty))

class HasText a where
  text :: a -> Text

instance HasText Text where
  text = id

instance HasText TextNonEmpty where
  text (TextNonEmpty f r) = f `cons` r

instance HasText B.SubAccountName where
  text = text . B.unSubAccountName

instance HasText B.SubCommodity where
  text = text . B.unSubCommodity

instance HasText B.Flag where
  text = text . B.unFlag

instance HasText B.Memo where
  text = text . B.unMemo

instance HasText B.Number where
  text = text . B.unNumber

instance HasText B.Payee where
  text = text . B.unPayee

instance HasText B.Tag where
  text = text . B.unTag
  
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

instance HasText a => HasTextList [a] where
  textList = map text
