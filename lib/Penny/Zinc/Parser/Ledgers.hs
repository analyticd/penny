module Penny.Zinc.Parser.Ledgers where

import Control.Applicative ((<$>), (<*>), pure, many, liftA)
import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as T

import qualified Penny.Copper as C
import qualified Penny.Liberty.Error as LE
import Penny.Lincoln.Boxes (TransactionBox, PriceBox)
import qualified Penny.Zinc.Error as ZE
import System.Console.MultiArg.Combinator (option)
import System.Console.MultiArg.Prim (ParserE, nextArg)
import System.IO (hIsTerminalDevice, stdin, stderr, hPutStrLn)

warnTerminal :: IO ()
warnTerminal =
  hPutStrLn stderr $ "zinc: warning: reading from standard input, "
  ++ "which is a terminal"

data Filename =
  Filename Text
  | Stdin

-- | Actually reads the file off disk. For now just let this crash if
-- any of the IO errors occur.
ledgerText :: Filename -> IO Text
ledgerText f = case f of
  Stdin -> do
    isTerm <- hIsTerminalDevice stdin
    when isTerm warnTerminal
    TIO.hGetContents stdin
  Filename fn -> TIO.readFile (unpack fn)

readLedgers :: NE.NonEmpty Filename
               -> IO (NE.NonEmpty (Filename, Text))
readLedgers = T.traverse f where
  f fn = (,) <$> pure fn <*> ledgerText fn

parseLedger ::
  C.DefaultTimeZone
  -> C.RadGroup
  -> (Filename, Text)
  -> Ex.Exceptional ZE.Error ([TransactionBox], [PriceBox])
parseLedger dtz rg (f, txt) = let
  fn = case f of
    Stdin -> pack "<stdin>"
    Filename x -> x
  in case C.parseTransactions dtz rg (C.Filename fn) txt of
    Ex.Exception e ->
      Ex.Exception (ZE.ParseError (pack . show $ e))
    Ex.Success is -> let
      folder i (ts, ps) = case i of
        C.Transaction t -> (t:ts, ps)
        C.Price p -> (ts, p:ps)
      in Ex.Success $ foldr folder ([], []) is

combineData ::
  F.Foldable f
  => f ([a], [b])
  -> ([a], [b])
combineData = F.foldr f ([], []) where
  f (as, bs) (ass, bss) = (as ++ ass, bs ++ bss)

filename :: ParserE LE.Error Filename
filename = do
  a <- nextArg
  return $ if a == pack "-"
           then Stdin
           else Filename a

filenames :: ParserE LE.Error (NE.NonEmpty Filename)
filenames = do
  fn1 <- option Stdin (Filename <$> nextArg)
  fns <- liftA (fmap Filename) (many nextArg)
  return $ NE.nonEmpty fn1 fns
