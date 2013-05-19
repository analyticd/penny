module Penny.Brenner.Database (mode) where

import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U
import qualified System.Console.MultiArg as MA

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " [global-options] database [local-options]"
  , "Shows the database of financial institution transactions."
  , "Does not accept any non-option arguments."
  , ""
  , "Local options:"
  , "  --help, -h Show this help and exit."
  ]

data Arg = ArgPos String deriving (Eq, Show)

mode :: MA.Mode (Maybe Y.FitAcct -> IO ())
mode = MA.Mode
  { MA.mName = "database"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = [ ]
  , MA.mPosArgs = return . ArgPos
  , MA.mProcess = processor
  , MA.mHelp = help
  }

processor
  :: [Arg]
  -> Maybe Y.FitAcct
  -> IO ()
processor ls mayFa
  | any isArgPos ls = fail $
        "penny-fit database: error: this command does"
        ++ " not accept non-option arguments."
  | otherwise = case mayFa of
      Nothing -> fail $ "no financial institution account"
        ++ " selected on command line, and no default"
        ++ " financial instititution account configured."
      Just fa -> do
        let dbLoc = Y.dbLocation fa
        db <- U.loadDb (Y.AllowNew False) dbLoc
        mapM_ putStr . map U.showDbPair $ db

isArgPos :: Arg -> Bool
isArgPos (ArgPos _) = True
