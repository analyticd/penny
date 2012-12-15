module Penny.Zinc.Parser.Ledgers (
    Filename
  , filenames
  , parseLedgers
  , readLedgers
  ) where

import Control.Applicative ((<$>), many, optional)
import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO

import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import qualified Penny.Zinc.Error as ZE
import System.Console.MultiArg.Prim (Parser, nextWord)
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

readLedgers :: [Filename] -> IO [(Filename, Text)]
readLedgers = mapM f where
  f fn = (\txt -> (fn, txt)) <$> ledgerText fn

parseLedgers
  :: [(Filename, Text)]
  -> Ex.Exceptional ZE.Error ([L.Transaction], [L.PricePoint])
parseLedgers ls =
  let toPair (f, t) = (convertFilename f, C.FileContents t)
      parsed = C.parse (map toPair ls)
      folder i (ts, ps) = case i of
        C.Transaction t -> (t:ts, ps)
        C.PricePoint p -> (ts, p:ps)
        _ -> (ts, ps)
      toResult (C.Ledger is) = foldr folder ([], []) is
      toErr x = ZE.ParseError . C.unErrorMsg $ x
  in Ex.mapExceptional toErr toResult parsed


filename :: Parser Filename
filename = f <$> nextWord
  where
    f a = if a == "-"
          then Stdin
          else Filename . pack $ a

filenames :: Parser [Filename]
filenames = do
  fn1 <- optional filename
  case fn1 of
    Nothing -> return [Stdin]
    Just fn -> do
      fns <- many filename
      return (fn:fns)
