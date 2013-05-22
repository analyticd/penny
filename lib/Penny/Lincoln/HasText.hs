module Penny.Lincoln.HasText where

import Data.Text (Text)
import qualified Data.Text as X

import qualified Penny.Lincoln.Bits as B

class HasText a where
  text :: a -> Text

instance HasText Text where
  text = id

instance HasText B.SubAccount where
  text = B.unSubAccount

instance HasText B.Flag where
  text = B.unFlag

instance HasText B.Commodity where
  text = B.unCommodity

instance HasText B.Number where
  text = B.unNumber

instance HasText B.Payee where
  text = B.unPayee

instance HasText B.Tag where
  text = B.unTag

instance HasText B.Filename where
  text = B.unFilename

class HasTextList a where
  textList :: a -> [Text]

-- | Wraps instances of HasTextList and provides a delimiter; the
-- result is an instance of HasText.
data Delimited a = Delimited
  { delimiter :: Text
  , delimited :: a
  } deriving (Eq, Show)

instance HasTextList a => HasTextList (Delimited a) where
  textList = textList . delimited

instance HasTextList a => HasText (Delimited a) where
  text a = X.intercalate (delimiter a) . textList . delimited $ a

instance HasTextList B.Account where
  textList = map text . B.unAccount

instance HasTextList B.Tags where
  textList = map text . B.unTags

instance HasTextList B.Memo where
  textList = B.unMemo
