{-# LANGUAGE OverloadedStrings #-}
-- | Parses any OFX 1.0-series file. Uses the parser from the ofx
-- package.
--
-- The Postings that this module returns /always/ have an empty
-- Description field. Information from the OFX Payee field is placed
-- into the Payee field of the Posting record.

module Penny.Brenner.OFX
  ( parser
  , prepassParser
  ) where

import Control.Applicative
import Data.List (isPrefixOf)
import qualified Data.OFX as O
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Brenner.Types as Y
import qualified Text.Parsec as P

-- | Parser for OFX files.
parser :: ( Y.ParserDesc, Y.ParserFn )
parser = prepassParser id


-- | Parser for OFX files.  Any incoming data is first filtered
-- through the given function.  This allows you to correct broken
-- OFX statements.  For example, Bank of America issues OFX files that
-- do not properly escape ampersands.  Using this function you can
-- change every ampersand to something properly escaped (or just
-- change it to the word \"and\".)
prepassParser :: (String -> String) -> ( Y.ParserDesc, Y.ParserFn )
prepassParser f = (Y.ParserDesc d, loadIncoming f)
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
  :: (String -> String)
  -- ^ Prepass function
  -> Y.FitFileLocation
  -> IO (Either String [Y.Posting])
loadIncoming pp (Y.FitFileLocation fn) = do
  contents <- fmap pp $ readFile fn
  return $
    ( either (Left . show) Right
      $ P.parse O.ofxFile fn contents )
    >>= O.transactions
    >>= mapM txnToPosting


txnToPosting
  :: O.Transaction
  -> Either String Y.Posting
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
      [] -> Left "empty amount"
      x:xs -> let str = if x == '-' || x == '+' then xs else amtStr
              in maybe (Left ("could not parse amount: " ++ amtStr))
                       Right $ Y.mkAmount str

