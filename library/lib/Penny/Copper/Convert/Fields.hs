module Penny.Copper.Convert.Fields where

import qualified Penny.Copper.Tree.Flag as T
import qualified Penny.Copper.Tree.Number as T
import qualified Penny.Copper.Tree.Payee.Posting as PP
import qualified Penny.Copper.Tree.Account.Unquoted as AU
import qualified Penny.Copper.Tree.Account.Quoted as AQ
import qualified Penny.Copper.Tree.Tag as T
import qualified Penny.Copper.Tree.Commodity as T
import qualified Penny.Copper.Tree.Side as T
import qualified Penny.Copper.Tree.Currency as T
import qualified Penny.Copper.Tree.Amount as T
import qualified Penny.Common as C
import qualified Penny.Posting as C
import qualified Penny.Numbers.Qty as C
import qualified Penny.Numbers.Abstract.Unsigned as C
import qualified Data.Text as X
import qualified Data.Foldable as F
import Penny.Numbers.Abstract.RadGroup
import Data.Sequence (Seq, (<|))
import Penny.Numbers.Natural
import Penny.Copper.Convert.Unsigned (toUnsigned)

packSeq
  :: (X.Text -> a)
  -> (b -> Char)
  -> Seq b
  -> a
packSeq ctor unpack sq = ctor . X.pack . F.toList . fmap unpack $ sq

toFlag :: T.Flag -> C.Flag
toFlag (T.Flag _ sq _) = packSeq C.Flag T.unFlagChar sq

toNumber :: T.Number -> C.Number
toNumber (T.Number _ sq _) = packSeq C.Number T.unNumberChar sq

postingPayeeToPayee :: PP.Payee -> C.Payee
postingPayeeToPayee (PP.Payee _ sq _) = packSeq C.Payee PP.unPayeeChar sq

unquotedAccountToAccount :: AU.Account -> C.Account
unquotedAccountToAccount (AU.Account (NE a1 as))
  = C.Account . flatten
  $ NE (unquotedFirstSubAccount a1)
       (fmap (unquotedNextSubAccount . snd) as)

unquotedFirstSubAccount :: AU.FirstSubAccount -> C.SubAccount
unquotedFirstSubAccount (AU.FirstSubAccount (NE c1 cs))
  = C.SubAccount . X.pack . F.toList
  $ (AU.unFirstChar c1) <| fmap AU.unNextChar cs

unquotedNextSubAccount :: AU.NextSubAccount -> C.SubAccount
unquotedNextSubAccount (AU.NextSubAccount ne)
  = C.SubAccount . X.pack . F.toList . fmap AU.unNextChar
  . flatten $ ne

quotedAccountToAccount :: AQ.Account -> C.Account
quotedAccountToAccount (AQ.Account _ (NE s1 ss) _)
  = C.Account . flatten
  $ NE (quotedSubAccount s1) (fmap (quotedSubAccount . snd) ss)

quotedSubAccount :: AQ.SubAccount -> C.SubAccount
quotedSubAccount (AQ.SubAccount sq) =
  packSeq C.SubAccount AQ.unSubAccountChar sq

toTag :: T.Tag -> C.Tag
toTag (T.Tag _ sq) = packSeq C.Tag T.unTagChar sq

debitToSide :: T.Debit -> C.Side
debitToSide (T.Debit _) = C.Debit

creditToSide :: T.Credit -> C.Side
creditToSide (T.Credit _) = C.Credit

toCommodity :: T.Commodity -> C.Commodity
toCommodity (T.Commodity _ sq _)
  = packSeq C.Commodity T.unCommodityChar sq

currencyToCommodity :: T.Currency -> C.Commodity
currencyToCommodity = C.Commodity . X.singleton . T.unCurrency

data ConvPreCurrency r = ConvPreCurrency
  { preCommodity :: C.Commodity
  , preUnsigned :: C.Unsigned r
  } deriving (Eq, Ord, Show)

preCurrency :: T.PreCurrency r -> ConvPreCurrency r
preCurrency (T.PreCurrency cy st)
  = ConvPreCurrency (currencyToCommodity cy) (toUnsigned st)

data ConvPostCurrency r = ConvPostCurrency
  { postUnsigned :: C.Unsigned r
  , postCommodity :: Maybe C.Commodity
  } deriving (Eq, Ord, Show)

postCurrency :: T.PostCurrency r -> ConvPostCurrency r
postCurrency (T.PostCurrency st mayCy) =
  ConvPostCurrency (toUnsigned st) (fmap currencyToCommodity mayCy)

data AmountConv r
  = ACLeft (ConvPreCurrency r)
  | ACRight (ConvPostCurrency r)
  deriving (Eq, Ord, Show)

amountPeriod :: T.AmountPeriod -> AmountConv Period
amountPeriod a = case a of
  T.APCurrency p -> ACLeft . preCurrency $ p
  T.APStart p -> ACRight . postCurrency $ p

amountComma :: T.AmountComma -> AmountConv Comma
amountComma (T.AmountComma _ ac2) = case ac2 of
  T.AC2Currency p _ -> ACLeft . preCurrency $ p
  T.AC2Start p _ -> ACRight . postCurrency $ p
