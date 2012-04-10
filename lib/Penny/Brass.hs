-- | Brass - the Happy Penny Parser

module Penny.Brass (
  Tr.RadGroup,
  Tr.periodComma,
  Tr.periodSpace,
  Tr.commaPeriod,
  Tr.commaSpace,
  Tr.DefaultTimeZone(DefaultTimeZone, unDefaultTimeZone),
  Tr.utcDefault,
  Tr.Error(..),
  Tr.Item(..),
  parse
  ) where


import Penny.Brass.Parser (brass)
import qualified Penny.Brass.Translator as Tr
import qualified Penny.Brass.Scanner as Sc
import qualified Penny.Lincoln as L
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.ByteString as BS

data Error = TranslatorError Tr.Error
             | ParseError String
             deriving Show

parse ::
  Tr.DefaultTimeZone
  -> Tr.RadGroup
  -> L.Filename
  -> BS.ByteString
  -> Ex.Exceptional Error [Tr.Item]
parse dtz rg fn bs =
  let st = Sc.State bs 1 1 1 1
      (parseRes, _) = Sc.runStateM brass st
  in case parseRes of
    Sc.Failed e -> Ex.Exception (ParseError e)
    Sc.Ok pf ->
      case Tr.pennyFile dtz rg fn pf of
        Ex.Exception e -> Ex.Exception (TranslatorError e)
        Ex.Success g -> return g
