module Penny.Cabin.Balance.Parser (parser) where

import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Control.Applicative ((<|>), (<$))
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Balance.Tree as Tree
import qualified Penny.Cabin.Options as CO
import qualified Penny.Liberty.Combinator as LC
import qualified Penny.Liberty.Error as E
import qualified Penny.Liberty.Types as LT
import qualified Penny.Shield as S
import qualified System.Console.MultiArg.Prim as P
import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg.Option as Opt

parser ::
  S.Runtime
  -> O.Options
  -> P.ParserE E.Error ([LT.PostingInfo] -> XL.Text, O.Options)
parser rt os = do
  command
  os' <- opts rt os
  let toTxt = Chk.bitsToText (O.colorPref os') . concat
              . Tree.report os'
  return (toTxt, os')

opts :: S.Runtime -> O.Options -> P.ParserE E.Error O.Options
opts rt os = let
  p o = color rt o
        <|> background o
        <|> showZero o
        <|> hideZero o
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
