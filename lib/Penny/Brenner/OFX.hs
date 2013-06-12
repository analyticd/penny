{-# LANGUAGE OverloadedStrings #-}
-- | Parses any OFX 1.0-series file. Uses the parser from the ofx
-- package.
--
-- The Postings that this module returns /always/ have an empty
-- Description field. Information from the OFX Payee field is placed
-- into the Payee field of the Posting record.

module Penny.Brenner.OFX (parser) where

import Control.Applicative
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (isPrefixOf)
import qualified Data.OFX as O
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Brenner.Types as Y
import qualified Text.Parsec as P

-- | Parser for OFX files.
parser :: ( Y.ParserDesc, Y.ParserFn )
parser = (Y.ParserDesc d, loadIncoming)
  where
    d = X.unlines
      [ "Parses OFX 1.0-series files."
      , "Open Financial Exchange (OFX) is a standard format"
      , "for providing financial information. It is documented"
      , "at http://www.ofx.net"
      , "This parser also handles QFX files, which are OFX"
      , "files with minor additions by the makers of Quicken."
      , "Many banks make this format available with the label"
      , "\"Download to Quicken\" or similar."
      ]

loadIncoming
  :: Y.FitFileLocation
  -> IO (Ex.Exceptional String [Y.Posting])
loadIncoming (Y.FitFileLocation fn) = do
  contents <- readFile fn
  return $
    ( Ex.mapException show
      . Ex.fromEither
      $ P.parse O.ofxFile fn contents )
    >>= O.transactions
    >>= mapM txnToPosting


txnToPosting
  :: O.Transaction
  -> Ex.Exceptional String Y.Posting
txnToPosting t = Y.Posting
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
      if "-" `isPrefixOf` amtStr then Y.Decrease else Y.Increase
    amt = case amtStr of
      [] -> Ex.throw "empty amount"
      x:xs -> let str = if x == '-' || x == '+' then xs else amtStr
              in Ex.fromMaybe ("could not parse amount: " ++ amtStr)
                 $ Y.mkAmount str

