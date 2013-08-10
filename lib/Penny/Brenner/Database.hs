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

data Arg = ArgPos String

mode :: Y.Mode
mode = MA.modeHelp
  "database"        -- Mode name
  help              -- Help function
  processor         -- Processing function
  []                -- Options
  MA.Intersperse    -- Interspersion
  (return . ArgPos) -- Posarg processor

processor
  :: Maybe Y.FitAcct
  -> [Arg]
  -> IO ()
processor mayFa ls
  | not . null $ ls = fail $
        "penny-fit database: error: this command does"
        ++ " not accept non-option arguments."
  | otherwise = do
        fa <- U.getFitAcct mayFa
        let dbLoc = Y.dbLocation fa
        db <- U.loadDb (Y.AllowNew False) dbLoc
        mapM_ putStr . map U.showDbPair $ db
