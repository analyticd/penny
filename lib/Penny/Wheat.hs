module Penny.Wheat
  ( Pdct
  , CaseSensitive
  , Pattern (..)
  , payee
  , number
  , flag
  , postingMemo
  , transactionMemo
  , CompUTC(..)
  , dateTime
  , CompQty(..)
  , qty
  , drCr
  , commodity
  , account
  , accountLevel
  , accountAny
  , tag
  ) where

import Control.Monad (mzero)
import qualified Penny.Steel.Prednote as N
import Penny.Steel.Prednote (pdct)
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln as L
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Data.Text.Encoding as XE
import qualified Text.Parsec as Parsec
import qualified Penny.Copper.Parsec as CP
import qualified Text.Regex.PCRE.Light as PCRE

type CaseSensitive = Bool

-- | Patterns to use for text matching.
data Pattern
  = Exact String CaseSensitive
    -- ^ Exactly match the given string
  | Regex String CaseSensitive
    -- ^ Match the Perl-compatible regular expression

  | Date (Maybe (CompUTC, T.UTCTime))
    -- ^ Subject must be a valid Copper dateTime. It is changed to a
    -- UTC time. If a (CompUTC, UTCTime) is provided, the DateTime
    -- must match the given expression.

  | MatchFunc String (X.Text -> Bool)
    -- ^ The given function must return True. The String describes the
    -- function.


type Regex = String
type Subject = X.Text
type Pdct = N.Pdct L.PostFam

matchPattern :: Pattern -> Subject -> Bool
matchPattern p s = case p of
  Exact pat cs ->
    let xpat = X.pack pat
    in if cs
       then X.toCaseFold xpat == X.toCaseFold s
       else xpat == s
  Regex pat cs -> pcreMatch cs pat s
  Date mayPat -> matchDate mayPat s
  MatchFunc _ func -> func s

matchDate :: Maybe (CompUTC, T.UTCTime) -> Subject -> Bool
matchDate mayPat s = fromMaybe False $ do
  d <- case Parsec.parse CP.dateTime "" s of
    Left _ -> mzero
    Right dt -> return dt
  case mayPat of
    Nothing -> return True
    Just (cu, utct) ->
      let cmp = compUTCtoCmp cu
      in return . (`cmp` utct) . L.toUTC $ d

descPattern :: Pattern -> String
descPattern p = case p of
  Exact s cs -> "the string \"" ++ s ++ "\"" ++ " "
    ++ if cs then "(case sensitive)" else "(case insensitive)"
  Regex s cs -> "the regular expression \"" ++ s ++ "\"" ++ " "
    ++ if cs then "(case sensitive)" else "(case insensitive)"
  Date mayPair -> "subject must be a valid date"
    ++ case mayPair of
        Nothing -> " (any date is OK)"
        Just (cu, utct) -> (" " ++ descUTC "string" cu utct)
  MatchFunc d _ -> d

descItem :: String -> Pattern -> String
descItem s p = s ++ " matches " ++ descPattern p

pcreMatch :: CaseSensitive -> Regex -> Subject -> Bool
pcreMatch cs r s = isJust $ PCRE.match rx bs runOpts
  where
    rx = flip PCRE.compile compOpts
         . XE.encodeUtf8 . X.pack $ r
    bs = XE.encodeUtf8 s
    runOpts = []
    compOpts = f [PCRE.utf8]
      where f = if cs then id else (PCRE.caseless :)

-- * Pattern matching fields

payee :: Pattern -> Pdct
payee p = pdct (descItem "payee" p) (P.payee (matchPattern p))


number :: Pattern -> Pdct
number p = pdct (descItem "number" p) (P.number (matchPattern p))

flag :: Pattern -> Pdct
flag p = pdct (descItem "flag" p) (P.flag (matchPattern p))

postingMemo :: Pattern -> Pdct
postingMemo p = pdct (descItem "posting memo" p)
                (P.postingMemo (matchPattern p))

transactionMemo :: Pattern -> Pdct
transactionMemo p = pdct (descItem "transaction memo" p)
                (P.transactionMemo (matchPattern p))

-- * UTC times

data CompUTC
  = UAfter
  | UOnOrAfter
  | UExactly
  | UBefore
  | UOnOrBefore

descUTC :: String -> CompUTC -> T.UTCTime -> String
descUTC desc c u = "date of " ++ desc ++ " is " ++ co ++ " " ++ dt
  where
    co = case c of
      UAfter -> "after"
      UOnOrAfter -> "on or after"
      UExactly -> "on"
      UBefore -> "before"
      UOnOrBefore -> "on or before"
    dt = show day ++ " " ++ hs ++ ":" ++ ms ++ ":" ++ ss
    T.UTCTime day difft = u
    T.TimeOfDay h m s = T.timeToTimeOfDay difft
    (hs, ms, ss) = (show h, show m, show (round s :: Int))

compUTCtoCmp :: Ord a => CompUTC -> a -> a -> Bool
compUTCtoCmp c = case c of
  UAfter -> (>)
  UOnOrAfter -> (>=)
  UExactly -> (==)
  UBefore -> (<)
  UOnOrBefore -> (<=)

dateTime :: CompUTC -> T.UTCTime -> Pdct
dateTime c t = pdct (descUTC "posting" c t) p
  where
    p = (`cmp` t) . L.toUTC . Q.dateTime
    cmp = compUTCtoCmp c

-- * Quantities

data CompQty
  = QGT
  | QGTEQ
  | QEQ
  | QLTEQ
  | QLT

descQty :: CompQty -> L.Qty -> String
descQty c q = "quantity of posting is " ++ co ++ " " ++ show q
  where
    co = case c of
      QGT -> "greater than"
      QGTEQ -> "greater than or equal to"
      QEQ -> "equal to"
      QLTEQ -> "less than or equal to"
      QLT -> "less than"

qty :: CompQty -> L.Qty -> Pdct
qty c q = pdct (descQty c q) p
  where
    p = (`cmp` q) . Q.qty
    cmp = case c of
      QGT -> (>)
      QGTEQ -> (>=)
      QEQ -> (==)
      QLTEQ -> (<=)
      QLT -> (<)

-- * DrCr

drCr :: L.DrCr -> Pdct
drCr dc = pdct ("posting is a " ++ desc) p
  where
    desc = case dc of
      L.Debit -> "debit"
      L.Credit -> "credit"
    p = (== dc) . Q.drCr

-- * Commodity

commodity :: Pattern -> Pdct
commodity p = pdct (descItem "commodity" p) (P.commodity (matchPattern p))

-- * Account name

account :: Pattern -> Pdct
account p = pdct (descItem "full account name" p)
            (P.account (X.singleton ':') (matchPattern p))

accountLevel :: Int -> Pattern -> Pdct
accountLevel i p = pdct (descItem ("sub-account " ++ show i) p)
                   (P.accountLevel i (matchPattern p))

accountAny :: Pattern -> Pdct
accountAny p = pdct (descItem "any sub-account" p)
               (P.accountAny (matchPattern p))

-- * Tags

tag :: Pattern -> Pdct
tag p = pdct (descItem "any tag" p) (P.tag (matchPattern p))

------------------------------------------------------------
-- AccountSpec
------------------------------------------------------------
