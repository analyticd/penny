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

textField
  :: HasText a
  => String
  -- ^ Name of field
  -> (Bundle -> a)
  -- ^ Gets the field from the bundle
  -> Predbox Text
  -> Predbox Bundle
textField n f = label n . contramap (text . f)

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

label :: String -> Predbox a -> Predbox a
label s = rename ((X.pack s <> X.pack " - ") <>)

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

tags :: Predbox Text -> Predbox Bundle
tags = undefined
