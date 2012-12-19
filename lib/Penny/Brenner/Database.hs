module Penny.Brenner.Database (mode) where

import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U
import qualified System.Console.MultiArg as MA
import qualified Control.Monad.Exception.Synchronous as Ex

help :: String
help = unlines
  [ "penny-fit [global-options] database [local-options]"
  , "Shows the database of financial institution transactions."
  , "Does not accept any non-option arguments."
  , ""
  , "Local options:"
  , "  --help, -h Show this help and exit."
  ]

data Arg = ArgHelp | ArgPos String deriving (Eq, Show)

mode
  :: Y.DbLocation
  -> MA.Mode String (Ex.Exceptional String (IO ()))
mode dbLoc = MA.Mode
  { MA.mId = "database"
  , MA.mName = "database"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = [MA.OptSpec ["help"] "h" (MA.NoArg ArgHelp)]
  , MA.mPosArgs = ArgPos
  , MA.mProcess = processor dbLoc
  }

processor
  :: Y.DbLocation
  -> [Arg]
  -> Ex.Exceptional String (IO ())
processor dbLoc ls = f
  where
    f | any (== ArgHelp) ls = return (putStrLn help)
      | any isArgPos ls = Ex.throw posArgError
      | otherwise = return showDb
    posArgError =
        "penny-fit database: error: this command does"
        ++ " not accept non-option arguments."
    showDb = do
      db <- U.quitOnError $ U.loadDb (Y.AllowNew False) dbLoc
      mapM_ putStr . map U.showDbPair $ db

isArgPos :: Arg -> Bool
isArgPos (ArgPos _) = True
isArgPos _ = False
