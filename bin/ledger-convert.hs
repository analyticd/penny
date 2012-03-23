-- | Converts a Ledger file to Penny.
--
-- Usage: ledger-convert filename where filename is the file to
-- convert.
--
-- Converted data is sent to stdout.
module Main where

import qualified Penny.Copper as C
import qualified Penny.Denver as D
import qualified Text.Parsec as P
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

dtz :: C.DefaultTimeZone
dtz = C.utcDefault

gs :: (C.GroupingSpec, C.GroupingSpec)
gs = (C.GroupAll, C.GroupAll)

rg :: C.RadGroup
rg = C.periodComma

main :: IO ()
main = do
  fn <- fmap head getArgs
  txt <- TIO.readFile fn
  let parsed = P.parse D.ledger fn txt
  items <- case parsed of
    Left e -> fail (show e)
    Right is -> return is
  rendered <- case C.renderItems dtz gs rg items of
    Nothing -> fail "rendering failed"
    Just r -> return r
  TIO.putStr rendered
