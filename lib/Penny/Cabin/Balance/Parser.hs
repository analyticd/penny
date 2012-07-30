module Penny.Cabin.Balance.Parser (parser) where

import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Control.Applicative ((<|>), many)
import Control.Monad ((>=>))
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
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import System.Console.MultiArg.Prim (Parser)
import qualified System.Console.MultiArg.Combinator as C
import qualified Text.Parsec as Parsec

data Error = BadColorName String
           | BadBackground String
           | BadCommodity String
           | BadDate String
             deriving Show

parser ::
  Parser (S.Runtime
          -> O.Options
          -> [L.Box Ly.LibertyMeta]
          -> [L.PricePoint]
          -> Ex.Exceptional X.Text XL.Text)
parser = do
  ls <- many opts
  let f rt opInit bs ps = do
        let ls' = map (\fn -> fn rt) ls
            errOpParsed = (foldl (>=>) return ls') opInit
        opParsed <- case errOpParsed of
          Ex.Exception e -> Ex.throw . X.pack . show $ e
          Ex.Success g -> return g
        bits <- Tree.report opParsed bs ps
        return
          . Chk.chunksToText (O.colorPref opParsed)
          . concat
          $ bits
  return f

processColorArg ::
  S.Runtime
  -> String
  -> Maybe Chk.Colors
processColorArg rt x
  | x == "yes" = return Chk.Colors8
  | x == "no" = return Chk.Colors0
  | x == "auto" = return Chk.Colors256
  | x == "256" = return (CO.maxCapableColors rt)
  | otherwise = Nothing

parseOpt :: [String] -> [Char] -> C.ArgSpec a -> Parser a
parseOpt ss cs a = C.parseOption [C.OptSpec ss cs a]

color :: Parser (S.Runtime
                 -> O.Options
                 -> Ex.Exceptional Error O.Options)
color = parseOpt ["color"] "" (C.OneArg f)
  where
    f a1 rt op = case processColorArg rt a1 of
      Nothing -> Ex.throw . BadColorName $ a1
      Just c -> return (op { O.colorPref = c })

processBackgroundArg ::
  String
  -> Maybe (Col.DrCrColors, Col.BaseColors)
processBackgroundArg x
  | x == "light" = return (LB.drCrColors, LB.baseColors)
  | x == "dark" = return (DB.drCrColors, DB.baseColors)
  | otherwise = Nothing


background :: Parser (O.Options -> Ex.Exceptional Error O.Options)
background = parseOpt ["background"] "" (C.OneArg f)
  where
    f a1 op = case processBackgroundArg a1 of
      Nothing -> Ex.throw . BadBackground $ a1
      Just (dc, base) ->
        return op { O.drCrColors = dc
                  , O.baseColors = base }

showZeroBalances ::
  Parser (O.Options -> Ex.Exceptional a O.Options)
showZeroBalances = parseOpt ["show-zero-balances"] "" (C.NoArg f)
  where
    f op =
      return (op {O.showZeroBalances = CO.ShowZeroBalances True })

hideZeroBalances ::
  Parser (O.Options -> Ex.Exceptional a O.Options)
hideZeroBalances = parseOpt ["hide-zero-balances"] "" (C.NoArg f)
  where
    f op =
      return (op {O.showZeroBalances = CO.ShowZeroBalances False })

convertLong ::
  Parser (O.Options -> Ex.Exceptional Error O.Options)
convertLong = parseOpt ["convert"] "" (C.TwoArg f)
  where
    f a1 a2 op = do
      cty <- case Parsec.parse CC.lvl1Cmdty "" (X.pack a1) of
        Left _ -> Ex.throw . BadCommodity $ a1
        Right g -> return g
      let parseDate = CD.dateTime (O.defaultTimeZone op)
      dt <- case Parsec.parse parseDate "" (X.pack a2) of
        Left _ -> Ex.throw . BadDate $ a2
        Right g -> return g
      let op' = op { O.convert = Just (cty, dt) }
      return op'

convertShort :: Parser (S.Runtime
                        -> O.Options
                        -> Ex.Exceptional Error O.Options)
convertShort = parseOpt [] ['c'] (C.OneArg f)
  where
    f a1 rt op = do
      cty <- case Parsec.parse CC.lvl1Cmdty "" (X.pack a1) of
        Left _ -> Ex.throw . BadCommodity $ a1
        Right g -> return g
      let dt = S.currentTime rt
          op' = op { O.convert = Just (cty, dt) }
      return op'
        

opts :: Parser (S.Runtime
                -> O.Options
                -> Ex.Exceptional Error O.Options)
opts =
  color
  <|> mkTwoArg background
  <|> mkTwoArg showZeroBalances
  <|> mkTwoArg hideZeroBalances
  <|> mkTwoArg convertLong
  <|> convertShort
  where
    mkTwoArg p = do
      f <- p
      return (\_ op -> f op)
