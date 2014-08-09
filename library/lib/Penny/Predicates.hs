{-# LANGUAGE OverloadedStrings #-}
module Penny.Predicates where

import qualified Prednote.Core as C
import Prednote
import Prednote.Format
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Tree as E
import Data.Monoid
import Penny.Common
import Penny.Serial
import Penny.TopLine
import Penny.Posting
import Data.Sequence
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Penny.Numbers.Natural
import Penny.Transaction
import Penny.Balanced
import Penny.Ent

rewrap
  :: Text
  -- ^ New static label
  -> (a -> Text)
  -- ^ Calculates new dynamic label.  The text here is prepended to
  -- the static label.  If you have nothing to add to the static
  -- label, have this return the empty Text.
  -> (a -> b)
  -> C.Pred b
  -> C.Pred a
rewrap st dyn f (C.Pred chkStat ev) = C.Pred chkStat' ev'
  where
    chkStat' = E.Node chk [chkStat]
      where
        chk = indentTxt st
    ev' a = E.Node out [child]
      where
        child = ev . f $ a
        lbl = E.rootLabel child
        out = lbl { C.dynamic = indentTxt lbl' }
        lbl'
          | X.null x = st
          | otherwise = x <> " - " <> st
          where
            x = dyn a

rewrapMaybe
  :: Text
  -- ^ New static label.  Should be something like @has 3
  -- sub-account(s)@ or the like.

  -> (a -> Text)
  -- ^ Calculates new dynamic label.  The text here is prepended to
  -- the static label.  If you have nothing to add to the static
  -- label, have this return the empty Text.

  -> (a -> Maybe b)
  -- ^ This function is applied to the input.  If the result is
  -- Nothing, the Pred is False.  Otherwise, the child Pred is applied
  -- to the resulting Just.

  -> Pred b
  -> Pred a

rewrapMaybe st dyn f (C.Pred chkStat ev) = C.Pred chkStat' ev'
  where
    chkStat' = E.Node (indentTxt st) [chkStat]
    ev' a = E.Node out child
      where
        dyn'
          | X.null txt = indentTxt st
          | otherwise = indentTxt $ txt <> " - " <> st
          where
            txt = dyn a

        (out, child) = case f a of
          Nothing -> (sc, [])
            where
              sc = C.Output False (C.Visible True) shrt dyn'
              shrt = Just shortCir

          Just b -> (o, [chld])
            where
              chld = ev b
              o = (E.rootLabel chld) { C.dynamic = dyn' }

commodity :: Pred Text -> Pred Commodity
commodity = rewrap "commodity" (const X.empty) unCommodity

memo :: Pred Text -> Pred Memo
memo = fanAny unMemo

number :: Pred Text -> Pred Number
number = rewrap "number" (const X.empty) unNumber

payee :: Pred Text -> Pred Payee
payee = rewrap "payee" (const X.empty) unPayee

flag :: Pred Text -> Pred Flag
flag = rewrap "flag" (const X.empty) unFlag

location :: Pred Int -> Pred Location
location = rewrap "location" (const X.empty) unLocation

clxn :: Pred Text -> Pred Clxn
clxn = rewrap "collection" (const X.empty) unClxn

forwardSer :: Pred Int -> Pred Serial
forwardSer = rewrap "forward serial" (const X.empty) forward

backwardSer :: Pred Int -> Pred Serial
backwardSer = rewrap "backward serial" (const X.empty) backward

globalSer :: Pred Serial -> Pred Serial
globalSer = rewrap "global serial" (const X.empty) id

clxnSer :: Pred Serial -> Pred Serial
clxnSer = rewrap "collection serial" (const X.empty) id

subAccount :: Pred Text -> Pred SubAccount
subAccount = rewrap "subaccount" (const X.empty) unSubAccount

anySubAccount :: Pred SubAccount -> Pred Account
anySubAccount = fanAny (F.toList . unAccount)

allSubAccount :: Pred SubAccount -> Pred Account
allSubAccount = fanAll (F.toList . unAccount)

indexSubAccount :: NonNeg -> Pred SubAccount -> Pred Account
indexSubAccount ix = rewrapMaybe st dyn f
  where
    st = "has at least " <> X.pack (show . succ . unNonNeg $ ix)
      <> " sub-account(s)"

    dyn a = "sub-account at index " <> X.pack (show . unNonNeg $ ix)
      <> " of account " <>
      (X.pack . show . F.toList . fmap unSubAccount . unAccount $ a)

    f a
      | unNonNeg ix < fromIntegral (S.length accts) =
          Just $ S.index accts (fromIntegral . unNonNeg $ ix)
      | otherwise = Nothing
      where
        accts = unAccount a

colonSeparatedAccount :: Pred Text -> Pred Account
colonSeparatedAccount =
  rewrap "colon-separated account" fmtAcct fmtAcct
  where
    fmtAcct = F.foldl' (<>) X.empty
      . intersperse ":"
      . fmap unSubAccount
      . unAccount

intersperse :: a -> Seq a -> Seq a
intersperse a sq = case viewl sq of
  EmptyL -> empty
  x :< xs -> x <| go xs
  where
    go as = case viewl as of
      EmptyL -> empty
      b :< bs -> a <| b <| go bs

tag :: Pred Text -> Pred Tag
tag = rewrap "tag" (const X.empty) unTag

anyTag :: Pred Tag -> Pred Tags
anyTag = fanAny (F.toList . unTags)

topLine :: (TopLine -> a) -> Pred a -> Pred TopLine
topLine f = rewrap "top line" (const X.empty) f

posting :: (Posting -> a) -> Pred a -> Pred Posting
posting f = rewrap "posting" (const X.empty) f

viewedPosting :: Pred Posting -> Pred (View Posting)
viewedPosting = rewrap "viewed posting" (const X.empty)
  (entMeta . viewCurrent)

anySiblingPosting :: Pred Posting -> Pred (View Posting)
anySiblingPosting = fanAny
  (F.toList . fmap entMeta . siblings)

allSiblingPostings :: Pred Posting -> Pred (View Posting)
allSiblingPostings = fanAll
  (F.toList . fmap entMeta . siblings)

bundle :: (Bundle -> a) -> Pred a -> Pred Bundle
bundle f = rewrap "bundle" (const X.empty) f

