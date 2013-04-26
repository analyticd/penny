-- | Parses any OFX 1.0-series file. Uses the parser from the ofx
-- package.

module Penny.Brenner.OFX (parser) where

import Control.Applicative
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (isPrefixOf)
import qualified Data.OFX as O
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Brenner.Types as Y
import qualified Text.Parsec as P


parser :: (String, Y.FitFileLocation
                   -> IO (Ex.Exceptional String [Y.Posting]))
parser = (help, loadIncoming)

help :: String
help = unlines
  [ "Parses any OFX 1.0-series file."
  , "Also parses QFX files, which are OFX files with minor"
  , "proprietary additions from Intuit."
  , "Many banks provide QFX files; they are typically labeled"
  , "as being for Quicken. Some banks also provide files"
  , "for the now-discontinued Microsoft Money; these often are"
  , "OFX files."
  , "Geared to work with deposit and credit card accounts. Although"
  , "your brokerage house might give you OFX files for your account"
  , "there, if those work with this parser, consider it a happy accident."
  , "This parser is intended for 1.0-series OFX files; 2.0 series files"
  , "are a bit different and they may or may not work."
  ]

loadIncoming :: Y.FitFileLocation
                -> IO (Ex.Exceptional String [Y.Posting])
loadIncoming (Y.FitFileLocation fn) = do
  contents <- readFile fn
  return $
    ( Ex.mapException show
      . Ex.fromEither
      $ P.parse O.ofxFile fn contents )
    >>= O.transactions
    >>= mapM txnToPosting


txnToPosting :: O.Transaction -> Ex.Exceptional String Y.Posting
txnToPosting t = Y.Posting
  <$> pure (Y.Date ( T.utctDay . T.zonedTimeToUTC
                   . O.txDTPOSTED $ t))
  <*> pure (Y.Desc X.empty)
  <*> pure incDec
  <*> amt
  <*> pure ( Y.Payee $ case O.txPayeeInfo t of
              Nothing -> X.empty
              Just ei -> case ei of
                Left d -> X.pack d
                Right p -> X.pack . O.peNAME $ p )
  <*> pure (Y.FitId . X.pack . O.txFITID $ t)
  where
    amtStr = O.txTRNAMT t
    incDec = if "-" `isPrefixOf` amtStr then Y.Decrease else Y.Increase
    amt = case amtStr of
      [] -> Ex.throw "empty amount"
      x:xs -> let str = if x == '-' || x == '+' then xs else amtStr
              in Ex.fromMaybe ("could not parse amount: " ++ amtStr)
                 $ Y.mkAmount str
