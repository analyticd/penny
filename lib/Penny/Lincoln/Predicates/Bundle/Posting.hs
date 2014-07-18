{-# LANGUAGE OverloadedStrings #-}
module Penny.Lincoln.Predicates.Bundle.Posting where

import qualified Penny.Lincoln.Queries.Bundle.Posting as Q
import Prednote
import qualified Prednote as P
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

textField
  :: HasText a
  => String
  -- ^ Name of field
  -> (Bundle -> a)
  -> Pred Text
  -> Pred Bundle
textField n f = P.wrap (X.pack n) dyn w
  where
    dyn a = X.pack n <> " - " <> X.pack (show . text . w $ a)
    w = text . f

textListField
  :: HasTextList a
  => String
  -- ^ Name of field
  -> (Bundle -> a)
  -> Pred [Text]
  -> Pred Bundle
textListField n f = P.wrap (X.pack n) dyn w
  where
    dyn a = X.pack n <> " - " <> X.pack (show . textList . w $ a)
    w = textList . f

ent :: Pred (Ent Posting) -> Pred Bundle
ent = P.wrap "ent" (const "ent") Q.ent

posting :: Pred Posting -> Pred Bundle
posting = P.wrap "posting" (const "posting") Q.posting

postingData :: Pred PostingData -> Pred Bundle
postingData = P.wrap "postingData" (const "postingData") Q.postingData

postingMeta :: Pred (Maybe PostingMeta) -> Pred Bundle
postingMeta = P.wrap "postingMeta" (const "postingMeta") Q.postingMeta

trio :: Pred Trio -> Pred Bundle
trio = P.wrap "trio" (const "trio") Q.trio

memo :: Pred Text -> Pred Bundle
memo = P.wrap "memo" (const "memo") f
  where
    f = X.concat . map (<> X.pack "\n") . unMemo . Q.memo

number :: Pred Text -> Pred Bundle
number = textField "number" Q.number

flag :: Pred Text -> Pred Bundle
flag = textField "flag" Q.flag

payee :: Pred Text -> Pred Bundle
payee = textField "payee" Q.payee

tags :: Pred [Text] -> Pred Bundle
tags = textListField "tags" Q.tags

qty
  :: RadGroup
  -- ^ Radix point and grouping character to use.  This is used only
  -- for displaying the Qty in the Pred.

  -> Pred Normal
  -> Pred Bundle
qty = undefined

{-
qty :: Pred Qty -> Pred Bundle
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

-}
