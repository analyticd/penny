module Penny.Cabin.Balance.Parser (parser) where

import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Control.Applicative ((<|>), (<$))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Balance.Tree as Tree
import qualified Penny.Cabin.Options as CO
import qualified Penny.Copper.Commodity as CC
import qualified Penny.Copper.DateTime as CD
import qualified Penny.Liberty.Combinator as LC
import qualified Penny.Liberty.Error as E
import qualified Penny.Liberty.Types as LT
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified System.Console.MultiArg.Prim as P
import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg.Option as Opt
import qualified Text.Parsec as Parsec

type ReportFn =
  [LT.PostingInfo]
  -> [L.PriceBox]
  -> Ex.Exceptional X.Text XL.Text

parser ::
  S.Runtime
  -> O.Options
  -> P.ParserE E.Error ReportFn
parser rt os = do
  command
  os' <- opts rt os
  let toTxt infos prices = do
        bits <- Tree.report os' infos prices
        return . Chk.bitsToText (O.colorPref os') . concat $ bits
  return toTxt

opts :: S.Runtime -> O.Options -> P.ParserE E.Error O.Options
opts rt os = let
  p o = color rt o
        <|> background o
        <|> showZero o
        <|> hideZero o
        <|> convertLong o
        <|> convertShort rt o
  in do
    ls <- LC.runUntilFailure p os
    case ls of
      [] -> return os
      xs -> return $ last xs

command :: P.ParserE E.Error ()
command = P.try $ do
  n <- P.nextArg
  if n == X.pack "bal" || n == X.pack "balance"
    then return ()
    else P.throw (E.UnexpectedWord (X.pack "balance") n)

color :: S.Runtime -> O.Options -> P.ParserE E.Error O.Options
color rt os = do
  (_, a) <- C.longOneArg (Opt.makeLongOpt (X.pack "color"))
  c <- processColorArg rt a
  return $ os { O.colorPref = c }

processColorArg ::
  S.Runtime
  -> X.Text
  -> P.ParserE E.Error Chk.Colors
processColorArg rt x
  | x == X.pack "yes" = return Chk.Colors8
  | x == X.pack "no" = return Chk.Colors0
  | x == X.pack "auto" = return Chk.Colors256
  | x == X.pack "256" = return (CO.maxCapableColors rt)
  | otherwise = P.throw (E.BadColorName x)

background :: O.Options -> P.ParserE E.Error O.Options
background os = do
  (_, a) <- C.longOneArg (Opt.makeLongOpt (X.pack "background"))
  (dc, bc) <- processBackgroundArg a
  return $ os { O.drCrColors = dc, O.baseColors = bc }

processBackgroundArg ::
  X.Text
  -> P.ParserE E.Error (Col.DrCrColors, Col.BaseColors)
processBackgroundArg x
  | x == X.pack "light" = return (LB.drCrColors, LB.baseColors)
  | x == X.pack "dark" = return (DB.drCrColors, DB.baseColors)
  | otherwise = P.throw (E.BadBackgroundArg x)

showZero :: O.Options -> P.ParserE E.Error O.Options
showZero os = os' <$ C.longNoArg lno where
  lno = Opt.makeLongOpt (X.pack "show-zero-balances")
  os' = os { O.showZeroBalances = CO.ShowZeroBalances True }

hideZero :: O.Options -> P.ParserE E.Error O.Options
hideZero os = os' <$ C.longNoArg lno where
  lno = Opt.makeLongOpt (X.pack "hide-zero-balances")
  os' = os { O.showZeroBalances = CO.ShowZeroBalances False }

convertLong :: O.Options -> P.ParserE E.Error O.Options
convertLong os = do
  (_, a1, a2) <- C.longTwoArg (Opt.makeLongOpt (X.pack "convert"))
  cty <- case Parsec.parse CC.lvl1Cmdty "" a1 of
    Left _ -> P.throw (E.BadCommodityError a1)
    Right g -> return g
  let parseDate = CD.dateTime (O.defaultTimeZone os)
  dt <- case Parsec.parse parseDate "" a2 of
    Left _ -> P.throw E.DateParseError
    Right g -> return g
  let os' = os { O.convert = Just (cty, dt) }
  return os'

convertShort ::
  S.Runtime
  -> O.Options
  -> P.ParserE E.Error O.Options
convertShort rt os = do
  (_, a1) <- C.shortOneArg (Opt.makeShortOpt 'c')
  cty <- case Parsec.parse CC.lvl1Cmdty "" a1 of
    Left _ -> P.throw (E.BadCommodityError a1)
    Right g -> return g
  let dt = S.currentTime rt
      os' = os { O.convert = Just (cty, dt) }
  return os'
