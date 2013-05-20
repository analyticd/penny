-- | Parses any OFX 1.0-series file. Uses the parser from the ofx
-- package.

module Penny.Brenner.OFX
  ( parser
  , DescSign(..)
  , ParserFn
  , parseOFXConfigFile
  ) where

import Control.Applicative
import Control.Arrow (first)
import qualified Control.Monad.Error as E
import Data.Functor.Identity (Identity(..))
import qualified Data.ConfigFile as CF
import Data.Char (toLower)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (isPrefixOf)
import qualified Data.OFX as O
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Brenner.Types as Y
import qualified Text.Parsec as P
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Lincoln as L
import qualified Penny.Copper.Render as R
import System.Environment (getProgName)
import qualified System.Exit as Exit
import qualified System.IO as IO

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
  -> (String, ParserFn)
parser help = (help, loadIncoming)

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

-- Uses ErrorT rather than Either because Either is an orphan instance
type Parser a = CF.ConfigParser -> E.ErrorT CF.CPError Identity a

type FitName = String

dbLocation :: FitName -> Parser Y.DbLocation
dbLocation n cf =
  let cf' = cf { CF.usedefault = False }
  in pullString "file" Y.DbLocation n cf'

pennyAcct :: FitName -> Parser Y.PennyAcct
pennyAcct = pullString "penny_acct" (Y.PennyAcct . Bd.account)

defaultAcct :: FitName -> Parser Y.DefaultAcct
defaultAcct = pullString "default_acct" (Y.DefaultAcct . Bd.account)

currency :: FitName -> Parser Y.Currency
currency = pullString "currency" (Y.Currency . L.Commodity)

type FieldName = String
groupSpec :: FieldName -> FitName -> Parser R.GroupSpec
groupSpec = pullList
  [ ("none", R.NoGrouping), ("large", R.GroupLarge),
    ("all", R.GroupAll) ]

groupSpecs :: FitName -> Parser R.GroupSpecs
groupSpecs n cf = R.GroupSpecs
  <$> groupSpec "group_left" n cf
  <*> groupSpec "group_right" n cf

translator :: FitName -> Parser Y.Translator
translator = pullList [ ("debit", Y.IncreaseIsDebit)
                      , ("credit", Y.IncreaseIsCredit) ]
                      "increase_is"

side :: FitName -> Parser L.Side
side = pullList [ ("left", L.CommodityOnLeft)
                , ("right", L.CommodityOnRight) ]
                "commodity_on"

spaceBetween :: FitName -> Parser L.SpaceBetween
spaceBetween = pullList [ ("false", L.NoSpaceBetween)
                        , ("true", L.SpaceBetween) ]
                        "space_between"

moreInfo :: FitName -> Parser String
moreInfo n cf = CF.get cf n "info"

pullList
  :: [(String, a)]
  -- ^ Mapping of config string to values

  -> String
  -- ^ Field name
  -> FitName
  -> Parser a
pullList ls field n p = do
  s <- fmap (map toLower) $ CF.get p n field
  case lookup s (map (first (map toLower)) ls) of
    Nothing -> fail $ "bad value for field " ++ field
                      ++ ": " ++ s
    Just v -> return v

pullString
  :: String
  -- ^ Section name
  -> (X.Text -> a)
  -- ^ Builds type
  -> FitName
  -> Parser a
pullString sect mkType n cp =
  fmap (mkType . X.pack)
  $ CF.get cp n sect

getParser
  :: FitName
  -> Parser (String, ParserFn)
getParser n cf = fmap parser $ moreInfo n cf

parseFitAcct :: FitName -> Parser Y.FitAcct
parseFitAcct n cf = Y.FitAcct
  <$> dbLocation n cf
  <*> pennyAcct n cf
  <*> defaultAcct n cf
  <*> currency n cf
  <*> groupSpecs n cf
  <*> translator n cf
  <*> side n cf
  <*> spaceBetween n cf
  <*> getParser n cf
  <*> pure (\_ (Y.Payee p) -> L.Payee p)

parseFitAccts :: Parser [(Y.Name, Y.FitAcct)]
parseFitAccts cf =
  let secs = CF.sections cf
      getAcct n = (,) <$> pure (Y.Name . X.pack $ n)
                      <*> parseFitAcct n cf
  in mapM getAcct secs

parseConfig :: Parser Y.Config
parseConfig cf = do
  accts <- parseFitAccts cf
  if CF.has_option cf "DEFAULT" "default_fit_acct"
    then do
      dflt <- fmap (Y.Name . X.pack)
              $ CF.get cf "DEFAULT" "default_fit_acct"
      case lookup dflt accts of
        Nothing -> fail $ "default financial institution account "
                        ++ "not found: "
                        ++ (X.unpack . Y.unName $ dflt)
        Just x -> return (Y.Config (Just (dflt, x)) accts)
    else return $ Y.Config Nothing accts

errExit :: Show e => Either e g -> IO g
errExit ei = do
  pn <- getProgName
  case ei of
    Left e -> do
      IO.hPutStrLn IO.stderr $
        pn ++ ": error: could not parse configuration file: "
           ++ show e
      Exit.exitFailure
    Right g -> return g

parseOFXConfigFile
  :: String
  -- ^ File location
  -> IO Y.Config
parseOFXConfigFile p =
  CF.readfile CF.emptyCP p
  >>= errExit . runIdentity . E.runErrorT
  >>= errExit . runIdentity . E.runErrorT . parseConfig
