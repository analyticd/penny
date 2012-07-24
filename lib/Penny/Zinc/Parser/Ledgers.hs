module Penny.Zinc.Parser.Ledgers where

import Control.Applicative ((<$>), (<*>), pure, many, liftA,
                            optional)
import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as T

import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import Penny.Lincoln.Boxes (TransactionBox, PriceBox)
import qualified Penny.Zinc.Error as ZE
import System.Console.MultiArg.Prim (Parser, nextArg)
import System.IO (hIsTerminalDevice, stdin, stderr, hPutStrLn)
import qualified Text.Parsec as Parsec

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

readLedgers :: [Filename] -> IO [(Filename, Text)]
readLedgers = mapM f where
  f fn = (,) <$> pure fn <*> ledgerText fn

parseLedger ::
  C.DefaultTimeZone
  -> C.RadGroup
  -> (Filename, Text)
  -> Ex.Exceptional ZE.Error ([TransactionBox], [PriceBox])
parseLedger dtz rg (f, txt) = let
  fnStr = case f of
    Stdin -> "<stdin>"
    Filename x -> unpack x
  fn = L.Filename . pack $ fnStr
  parser = C.ledger fn dtz rg
  in case Ex.fromEither $ Parsec.parse parser fnStr txt of
    Ex.Exception e ->
      Ex.Exception (ZE.ParseError (pack . show $ e))
    Ex.Success (C.Ledger is) -> let
      folder i (ts, ps) = case snd i of
        C.Transaction t -> (t:ts, ps)
        C.Price p -> (ts, p:ps)
        _ -> (ts, ps)
      in Ex.Success $ foldr folder ([], []) is

combineData ::
  F.Foldable f
  => f ([a], [b])
  -> ([a], [b])
combineData = F.foldr f ([], []) where
  f (as, bs) (ass, bss) = (as ++ ass, bs ++ bss)

filename :: Parser Filename
filename = f <$> nextArg
  where
    f a = if a == pack "-"
          then Stdin
          else Filename a

filenames :: Parser [Filename]
filenames = do
  fn1 <- optional filename
  case fn1 of
    Nothing -> return [Stdin]
    Just fn -> do
      fns <- many filename
      return fn1:fns
