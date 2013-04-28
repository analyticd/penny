-- | Parses any OFX 1.0-series file. Uses the parser from the ofx
-- package.

module Penny.Brenner.OFX (parser, DescSign(..), ParserFn) where

import Control.Applicative
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (isPrefixOf)
import qualified Data.OFX as O
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Brenner.Types as Y
import qualified Text.Parsec as P


type ParserFn
  = Y.FitFileLocation
  -> IO (Ex.Exceptional String [Y.Posting])

-- | Do positive amounts increase or decrease the balance of the
-- account? According to the OFX spec, amounts should always be
-- positive if (from the customer's perspective) they increase the
-- balance of the account, but not all OFX providers conform to this.
data DescSign
  = PosIsIncrease
  | PosIsDecrease

parser
  :: String
  -- ^ Help string
  -> DescSign
  -> (String, ParserFn)
parser help d = (help, loadIncoming d)

loadIncoming
  :: DescSign
  -> Y.FitFileLocation
  -> IO (Ex.Exceptional String [Y.Posting])
loadIncoming d (Y.FitFileLocation fn) = do
  contents <- readFile fn
  return $
    ( Ex.mapException show
      . Ex.fromEither
      $ P.parse O.ofxFile fn contents )
    >>= O.transactions
    >>= mapM (txnToPosting d)


txnToPosting
  :: DescSign
  -> O.Transaction
  -> Ex.Exceptional String Y.Posting
txnToPosting d t = Y.Posting
  <$> pure (Y.Date ( T.utctDay . T.zonedTimeToUTC
                   . O.txDTPOSTED $ t))
  <*> pure (Y.Desc X.empty)
  <*> pure incDec
  <*> amt
  <*> pure ( Y.Payee $ case O.txPayeeInfo t of
              Nothing -> X.empty
              Just ei -> case ei of
                Left x -> X.pack x
                Right p -> X.pack . O.peNAME $ p )
  <*> pure (Y.FitId . X.pack . O.txFITID $ t)
  where
    amtStr = O.txTRNAMT t
    incDec =
      if "-" `isPrefixOf` amtStr
      then case d of
            PosIsIncrease -> Y.Decrease
            PosIsDecrease -> Y.Increase
      else case d of
            PosIsIncrease -> Y.Increase
            PosIsDecrease -> Y.Decrease
    amt = case amtStr of
      [] -> Ex.throw "empty amount"
      x:xs -> let str = if x == '-' || x == '+' then xs else amtStr
              in Ex.fromMaybe ("could not parse amount: " ++ amtStr)
                 $ Y.mkAmount str
