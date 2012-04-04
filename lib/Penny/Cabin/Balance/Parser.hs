module Penny.Cabin.Balance.Parser where

import qualified Data.Text as X
import Control.Applicative ((<|>))
import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Balance.Tree as Tree
import qualified Penny.Liberty.Combinator as LC
import qualified Penny.Liberty.Error as E
import qualified Penny.Liberty.Types as LT
import qualified System.Console.MultiArg.Prim as P
import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg.Option as Opt

parser ::
  O.Options
  -> P.ParserE E.Error ([LT.PostingInfo] -> Chk.Chunk, O.Options)
parser os = do
  command
  os' <- opts os
  return (Tree.report os', os')

opts :: O.Options -> P.ParserE E.Error O.Options
opts os = let
  p o = color o
        <|> background o
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

color :: O.Options -> P.ParserE E.Error O.Options
color os = do
  (_, a) <- C.longOneArg (Opt.makeLongOpt (X.pack "color"))
  c <- processColorArg a
  return $ os { O.colorPref = c }

processColorArg :: X.Text -> P.ParserE E.Error Chk.ColorPref
processColorArg x
  | x == X.pack "yes" = return Chk.Pref8
  | x == X.pack "no" = return Chk.Pref0
  | x == X.pack "auto" = return Chk.PrefAuto
  | x == X.pack "256" = return Chk.Pref256
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

