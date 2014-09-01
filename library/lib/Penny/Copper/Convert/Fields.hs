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
import qualified Penny.Common as C
import qualified Penny.Posting as C
import qualified Penny.Numbers.Qty as C
import qualified Data.Text as X
import qualified Data.Foldable as F
import Data.Sequence (Seq, (<|))
import Penny.Numbers.Natural

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
