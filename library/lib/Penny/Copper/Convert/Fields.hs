module Penny.Copper.Convert.Fields where

import qualified Penny.Copper.Tree.Date as T
import qualified Penny.Copper.Tree.Digit as T
import qualified Penny.Copper.Tree.Digits as T
import qualified Penny.Copper.Tree.Flag as T
import qualified Penny.Copper.Tree.Number as T
import qualified Penny.Copper.Tree.Payee.Posting as PP
import qualified Penny.Copper.Tree.Payee.TopLine as PT
import qualified Penny.Copper.Tree.Account.Unquoted as AU
import qualified Penny.Copper.Tree.Account.Quoted as AQ
import qualified Penny.Copper.Tree.Tag as T
import qualified Penny.Copper.Tree.Commodity as T
import qualified Penny.Copper.Tree.Side as T
import qualified Penny.Copper.Tree.Currency as T
import qualified Penny.Copper.Tree.Amount as T
import qualified Penny.Copper.Tree.Tokens as T
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
import qualified Data.Time as Time
import qualified Penny.DateTime as C
import qualified Data.Sums as S
import Control.Monad

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

topLinePayeeToPayee :: PT.Payee -> C.Payee
topLinePayeeToPayee (PT.Payee (NE c1 cs))
  = C.Payee $ PT.unPayeeFirstChar c1 `X.cons`
  (X.pack . F.toList . fmap PT.unPayeeNextChar $ cs)

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

digits4 :: T.Digits4 -> Int
digits4 (T.Digits4 d3 d2 d1 d0)
  = di d3 * 1000
  + di d2 * 100
  + di d1 * 10
  + di d0
  where
    di = T.digitToInt

digits1or2 :: T.Digits1or2 -> Int
digits1or2 (T.Digits1or2 x y) = case y of
  Nothing -> di x
  Just d0 -> di x * 10 + di d0
  where
    di = T.digitToInt

data DateError
  = InvalidDate
  | InvalidHours
  | InvalidMinutes
  | InvalidSeconds
  | InvalidOffset
  deriving (Eq, Ord, Show)

date :: T.Date -> Either DateError Time.Day
date (T.Date yd _ md _ dd) = maybe (Left InvalidDate)
  Right $ Time.fromGregorianValid y m d
  where
    y = fromIntegral $ digits4 yd
    m = digits1or2 md
    d = digits1or2 dd

hours :: T.Digits1or2 -> Either DateError C.Hours
hours = maybe (Left InvalidHours) Right . C.intToHours
  . digits1or2

minutes :: T.Digits1or2 -> Either DateError C.Minutes
minutes = maybe (Left InvalidMinutes) Right . C.intToMinutes
  . digits1or2

seconds :: T.Digits1or2 -> Either DateError C.Seconds
seconds = maybe (Left InvalidSeconds) Right . C.intToSeconds
  . digits1or2

timeZone :: T.TimeZone -> Either DateError C.TimeZoneOffset
timeZone (T.TimeZone hp d4 _) = maybe (Left InvalidOffset) Right
  . C.minsToOffset . doSign . digits4 $ d4
  where
    doSign = case hp of
      S.S2a T.Hyphen -> negate
      S.S2b T.Plus -> id

time
  :: T.Time
  -> Either DateError (C.Hours, C.Minutes, C.Seconds, C.TimeZoneOffset)
time (T.Time _ dH _ dM t2) = do
  h <- hours dH
  m <- minutes dM
  (s, o) <- case t2 of
    T.Time2End _ -> return (C.zeroSeconds, C.noOffset)
    T.Time2Space _ t3 -> time3 t3
    T.Time2Next t3 -> time3 t3
  return (h, m, s, o)

time3 :: T.Time3 -> Either DateError (C.Seconds, C.TimeZoneOffset)
time3 t3 = case t3 of
  T.Time3Seconds _ dS t4 -> liftM2 (,) (seconds dS) (time4 t4)
  T.Time3Zone tz -> liftM2 (,) (return C.zeroSeconds) (timeZone tz)

time4 :: T.Time4 -> Either DateError C.TimeZoneOffset
time4 t4 = case t4 of
  T.Time4Zone tz -> timeZone tz
  T.Time4Space _ tz -> timeZone tz
  T.Time4End _ -> return C.noOffset

dateTime :: T.Date -> Maybe T.Time -> Either DateError C.DateTime
dateTime d mt = do
  dy <- date d
  (h, m, s, o) <- case mt of
    Nothing -> return
      (C.zeroHours, C.zeroMinutes, C.zeroSeconds, C.noOffset)
    Just t -> time t
  return $ C.DateTime dy h m s o
