-- | Brenner - Penny financial institution interfaces
--
-- Brenner provides a uniform way to interact with downloaded data
-- from financial Given a parser, Brenner will import the transactions
-- and store them in a database. From there it is easy to merge the
-- transactions (without duplicates) into a ledger file, and then to
-- clear transactions from statements in an automated fashion.
module Penny.Brenner
  ( FitAcct(..)
  , Config(..)
  , R.GroupSpecs(..)
  , R.GroupSpec(..)
  , Y.Translator(..)
  , L.Side(..)
  , L.SpaceBetween(..)
  , brennerMain
  ) where

import qualified Penny.Brenner.Types as Y
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Copper.Render as R
import qualified Penny.Brenner.Clear as C
import qualified Penny.Brenner.Database as D
import qualified Penny.Brenner.Import as I
import qualified Penny.Brenner.Merge as M
import qualified Penny.Brenner.Print as P
import Control.Applicative ((<*>))
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex

brennerMain :: Config -> IO ()
brennerMain cf = do
  let cf' = convertConfig cf
  ioAction <- MA.modesWithHelp (help cf') globalOpts
    (preProcessor cf')
  ioAction

data Arg = AFitAcct String
  deriving (Eq, Show)

toFitAcctOpt :: Arg -> Maybe String
toFitAcctOpt a = case a of { AFitAcct s -> Just s }

globalOpts :: [MA.OptSpec Arg]
globalOpts =
  [ MA.OptSpec ["fit-account"] "f" (MA.OneArg AFitAcct) ]

preProcessor
  :: Y.Config -> [Arg] -> Either (a -> IO ()) [MA.Mode (IO ())]
preProcessor cf as = Ex.toEither . Ex.mapException (const . fail) $ do
  let mayFiStr = case mapMaybe toFitAcctOpt as of
        [] -> Nothing
        xs -> Just . last $ xs
  fi <- case mayFiStr of
    Nothing -> return $ Y.defaultFitAcct cf
    Just s ->
      let pdct (Y.Name n, _) = n == X.pack s
      in case filter pdct (Y.moreFitAccts cf) of
           [] -> Ex.throw $
              "financial institution account "
              ++ s ++ " not configured."
           (_, c):[] -> return $ Just c
           _ -> Ex.throw $
              "more than one financial institution account "
              ++ "named " ++ s ++ " configured."
  return $ [C.mode, I.mode, M.mode, P.mode, D.mode] <*> [fi]

help
  :: Y.Config
  -> String
  -- ^ Program name

  -> String
help c n = unlines ls ++ cs
  where
    ls = [ "usage: " ++ n ++ " [global-options]"
            ++ " COMMAND [local-options]"
            ++ " ARGS..."
         , ""
         , "where COMMAND is one of:"
         , "import, merge, clear, database, print"
         , ""
         , "For help on an individual command and its"
           ++ " local options, use "
         , n ++ " COMMAND --help"
         , ""
         , "Global Options:"
         , "-f, --fit-account ACCOUNT"
         , "  Use one of the Additional Financial Institution"
         , "  Accounts shown below. If this option does not appear,"
         , "  the default account is used if there is one."
         , "-h, --help"
         , "  Show help and exit"
         , ""
         ]
    showPair (Y.Name a, cd) = "Additional financial institution "
      ++ "account: " ++ X.unpack a ++ "\n" ++ showFitAcct cd
    cs = showDefaultFitAcct (Y.defaultFitAcct c)
         ++ more
    more = if null (Y.moreFitAccts c)
           then "No additional financial institution accounts\n"
           else concatMap showPair . Y.moreFitAccts $ c

showDefaultFitAcct :: Maybe Y.FitAcct -> String
showDefaultFitAcct mc = case mc of
  Nothing -> "No default financial institution account\n"
  Just c -> "Default financial institution account:\n" ++ showFitAcct c

label :: String -> String -> String
label l o = "  " ++ l ++ ": " ++ o ++ "\n"

showAccount :: L.Account -> String
showAccount =
  X.unpack
  . X.intercalate (X.singleton ':')
  . map L.unSubAccount
  . L.unAccount

showFitAcct :: Y.FitAcct -> String
showFitAcct c =
  label "Database location"
    (X.unpack . Y.unDbLocation . Y.dbLocation $ c)

  ++ label "Penny account"
     (showAccount . Y.unPennyAcct . Y.pennyAcct $ c)

  ++ label "Account for new offsetting postings"
     (showAccount . Y.unDefaultAcct . Y.defaultAcct $ c)

  ++ label "Currency"
     (X.unpack . L.unCommodity . Y.unCurrency . Y.currency $ c)

  ++ "\n"

  ++ "More information about the parser:\n"
  ++ (fst . Y.parser $ c)
  ++ "\n\n"


-- | Information to configure a single financial institution account.
data FitAcct = FitAcct
  { dbLocation :: String
    -- ^ Path and filename to where the database is kept. You can use
    -- an absolute or relative path (if it is relative, it will be
    -- resolved relative to the current directory at runtime.)

  , pennyAcct :: String
    -- ^ The account that you use in your Penny file to hold
    -- transactions for this card. Separate each sub-account with
    -- colons (as you do in the Penny file.)

  , defaultAcct :: String
    -- ^ When new transactions are created, one of the postings will
    -- be in the amexAcct given above. The other posting will be in
    -- this account.

  , currency :: String
    -- ^ The commodity for the currency of your card (e.g. @$@).

  , groupSpecs :: R.GroupSpecs
    -- ^ How to group digits when printing the resulting ledger. All
    -- quantities (not just those affected by this program) will be
    -- formatted using this specification.

  , translator :: Y.Translator
    -- ^ See the documentation under the 'Translator' type for
    -- details.

  , side :: L.Side
  -- ^ When creating new transactions, the commodity will be on this
  -- side

  , spaceBetween :: L.SpaceBetween
  -- ^ When creating new transactions, is there a space between the
  -- commodity and the quantity

  , parser :: ( String
              , Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -- ^ Parses a file of transactions from the financial
  -- institution. The function must open the file and parse it. This
  -- is in the IO monad not only because the function must open the
  -- file itself, but also so the function can perform arbitrary IO
  -- (run pdftotext, maybe?) If there is failure, the function can
  -- return an Exceptional String, which is the error
  -- message. Alternatively the function can raise an exception in the
  -- IO monad (currently Brenner makes no attempt to catch these) so
  -- if any of the IO functions throw you can simply not handle the
  -- exceptions.
  --
  -- The first element of the pair is a help string which should
  -- indicate how to download the data, as a helpful reminder.


  } deriving Show

convertFitAcct :: FitAcct -> Y.FitAcct
convertFitAcct (FitAcct db ax df cy gs tl sd sb ps) = Y.FitAcct
  { Y.dbLocation = Y.DbLocation . X.pack $ db
  , Y.pennyAcct = Y.PennyAcct . Bd.account . X.pack $ ax
  , Y.defaultAcct = Y.DefaultAcct . Bd.account . X.pack $ df
  , Y.currency = Y.Currency . L.Commodity . X.pack $ cy
  , Y.groupSpecs = gs
  , Y.translator = tl
  , Y.side = sd
  , Y.spaceBetween = sb
  , Y.parser = ps
  }

data Config = Config
  { defaultFitAcct :: Maybe FitAcct
  , moreFitAccts :: [(String, FitAcct)]
  } deriving Show

convertConfig :: Config -> Y.Config
convertConfig (Config d m) = Y.Config
  { Y.defaultFitAcct = fmap convertFitAcct d
  , Y.moreFitAccts =
      let f (n, c) = (Y.Name (X.pack n), convertFitAcct c)
      in map f m
  }
