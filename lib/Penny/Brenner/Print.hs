-- | Prints parsed transactions.
--
-- TODO add support to this and other Brenner components for reading
-- from stdin.
module Penny.Brenner.Print where

import qualified Penny.Brenner.Types as Y
import qualified System.Console.MultiArg as MA
import qualified System.IO as IO
import qualified System.Exit as E
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe)
import qualified Data.Text as X

help :: String
help = unlines
  [ "penny-fit [global-options] print FILE..."
  , "Parses the transactions in each FILE using the appropriate parser"
  , "and prints the parse result to standard output."
  ]

data Arg
  = ArgHelp
  | ArgFile String
  deriving (Eq, Show)

mode
  :: (Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -> MA.Mode String (Ex.Exceptional String (IO ()))
mode prsr = MA.Mode
  { MA.mId = "print"
  , MA.mName = "print"
  , MA.mIntersperse = MA.Intersperse
  , MA.mOpts = [MA.OptSpec ["help"] "h" (MA.NoArg ArgHelp)]
  , MA.mPosArgs = ArgFile
  , MA.mProcess = processor prsr
  }

processor
  :: (Y.FitFileLocation -> IO (Ex.Exceptional String [Y.Posting]))
  -> [Arg]
  -> Ex.Exceptional String (IO ())
processor prsr ls =
  if any (== ArgHelp) ls
  then return (putStrLn help)
  else return (mapM_ f . mapMaybe toFile $ ls)
  where
    f file = do
      r <- prsr file
      case r of
        Ex.Exception s -> do
          IO.hPutStrLn IO.stderr $ "penny-fit import: error: " ++ s
          E.exitFailure
        Ex.Success ps -> mapM putStr . map toString $ ps
    toFile a = case a of
      ArgFile s -> Just (Y.FitFileLocation s)
      _ -> Nothing

label :: String -> X.Text -> String
label s x = s ++ ": " ++ X.unpack x ++ "\n"

toString :: Y.Posting -> String
toString (Y.Posting dt dc nc am py fd) =
  label "Date" (X.pack . show . Y.unDate $ dt)
  ++ label "Description" (Y.unDesc dc)
  ++ label "Type" (X.pack $ case nc of
                    Y.Increase -> "increase"
                    Y.Decrease -> "decrease")
  ++ label "Amount" (Y.unAmount am)
  ++ label "Payee" (Y.unPayee py)
  ++ label "Financial institution ID" (Y.unFitId fd)
  ++ "\n"
