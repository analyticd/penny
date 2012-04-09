module Main where

import Penny.Brass.Parser (brass)
import qualified Penny.Lincoln.Strict as Strict
import qualified Data.ByteString as BS
import qualified Penny.Brass.Start as T
import qualified Penny.Brass.Scanner as S
--import Text.Show.Pretty (ppShow)

parser :: S.StateM T.PennyFile
parser = brass

main :: IO ()
main = do
  bs <- BS.getContents
  let st = S.State bs 1 1 1 1
      c = S.runStateM parser st
  case fst c of
    S.Ok r -> print . T.revPennyFile $ r
    S.Failed s -> print s >> (print . snd $ c)

