module Penny.Lincoln.Predicates.Bundle.Posting where

import qualified Penny.Lincoln.Queries.Bundle.Posting as Q
import Data.Prednote.Predbox
import Penny.Lincoln.HasText
import Penny.Lincoln.Transaction
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as X
import Data.Functor.Contravariant
import Penny.Lincoln.Ents
import Penny.Lincoln.Posting
import Penny.Lincoln.Pieces
import Penny.Lincoln.Trio
import Penny.Lincoln.Decimal
import Penny.Lincoln.Common
import Penny.Lincoln.Serial

label :: String -> Predbox a -> Predbox a
label s = rename ((X.pack s <> X.pack " - ") <>)

textField
  :: HasText a
  => String
  -- ^ Name of field
  -> (Bundle -> a)
  -- ^ Gets the field from the bundle
  -> Predbox Text
  -> Predbox Bundle
textField n f = label n . contramap (text . f)

textListField
  :: HasTextList a
  => String
  -> (Bundle -> a)
  -> Predbox [Text]
  -> Predbox Bundle
textListField n f = label n . contramap (textList . f)

ent :: Predbox (Ent Posting) -> Predbox Bundle
ent = contramap Q.ent

posting :: Predbox Posting -> Predbox Bundle
posting = contramap Q.posting

postingData :: Predbox PostingData -> Predbox Bundle
postingData = contramap Q.postingData

postingMeta :: Predbox (Maybe PostingMeta) -> Predbox Bundle
postingMeta = contramap Q.postingMeta

trio :: Predbox Trio -> Predbox Bundle
trio = contramap Q.trio

memo :: Predbox Text -> Predbox Bundle
memo = label "memo" . contramap f
  where
    f = X.concat . map (<> X.pack "\n") . unMemo . Q.memo

number :: Predbox Text -> Predbox Bundle
number = textField "number" Q.number

flag :: Predbox Text -> Predbox Bundle
flag = textField "flag" Q.flag

payee :: Predbox Text -> Predbox Bundle
payee = textField "payee" Q.payee

tags :: Predbox [Text] -> Predbox Bundle
tags = textListField "tags" Q.tags

qty :: Predbox Qty -> Predbox Bundle
qty = label "qty" . contramap Q.qty

commodity :: Predbox Text -> Predbox Bundle
commodity = textField "commodity" Q.commodity

entrio :: Predbox Entrio -> Predbox Bundle
entrio = label "entrio" . contramap Q.entrio

account :: Predbox [Text] -> Predbox Bundle
account = textListField "account" Q.account

line :: Predbox (Maybe Int) -> Predbox Bundle
line = label "line" . contramap (fmap unLine . Q.line)

globalSerial :: Predbox (Maybe Serial) -> Predbox Bundle
globalSerial = label "global serial" . contramap Q.globalSerial

fileSerial :: Predbox (Maybe Serial) -> Predbox Bundle
fileSerial = label "file serial" . contramap Q.fileSerial

