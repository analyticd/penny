module Penny.Zinc.Parser.Ledgers (
    Filename
  , parseLedgers
  , readLedgers
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO

import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import System.IO (hIsTerminalDevice, stdin, stderr, hPutStrLn)


warnTerminal :: IO ()
warnTerminal =
  hPutStrLn stderr $ "zinc: warning: reading from standard input, "
  ++ "which is a terminal"

data Filename =
  Filename Text
  | Stdin

-- | Converts a Ledgers filename to a Lincoln filename.
convertFilename :: Filename -> L.Filename
convertFilename (Filename x) = L.Filename x
convertFilename Stdin = L.Filename . pack $ "<stdin>"

-- | Actually reads the file off disk. For now just let this crash if
-- any of the IO errors occur.
ledgerText :: Filename -> IO Text
ledgerText f = case f of
  Stdin -> do
    isTerm <- hIsTerminalDevice stdin
    when isTerm warnTerminal
    TIO.hGetContents stdin
  Filename fn -> TIO.readFile (unpack fn)

-- | Converts a string from the command line to a Filename.
toFilename :: String -> Filename
toFilename s =
  if s == "-"
  then Stdin
  else Filename . pack $ s

readLedgers :: [String] -> IO [(Filename, Text)]
readLedgers ss =
  let fns = if null ss then [Stdin] else map toFilename ss
      f fn = (\txt -> (fn, txt)) <$> ledgerText fn
  in mapM f fns


parseLedgers
  :: [(Filename, Text)]
  -> Ex.Exceptional String ([L.Transaction], [L.PricePoint])
parseLedgers ls =
  let toPair (f, t) = (convertFilename f, C.FileContents t)
      parsed = C.parse (map toPair ls)
      folder i (ts, ps) = case i of
        C.Transaction t -> (t:ts, ps)
        C.PricePoint p -> (ts, p:ps)
        _ -> (ts, ps)
      toResult (C.Ledger is) = foldr folder ([], []) is
      toErr x = "could not parse ledger: "
                ++ (unpack . C.unErrorMsg $ x)
  in Ex.mapExceptional toErr toResult parsed

