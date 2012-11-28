-- | penny-convert reads one or more filenames from the command
-- line. These files are read in and then rendered.

module Main where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Copper as C
import qualified Penny.Copper.Item as I
import qualified Penny.Lincoln as L
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import qualified Data.Text as X

-- | Do not change the grouping spec; no attempt was made to make
-- the old renderer properly render the thin spaces.
groupingSpec :: (C.GroupingSpec, C.GroupingSpec)
groupingSpec = (C.NoGrouping, C.NoGrouping)

-- | Change this to suit the input in your file
radGroup :: C.RadGroup
radGroup = C.periodComma

-- | Change to suit the input in your file.
defaultTimeZone :: C.DefaultTimeZone
defaultTimeZone = C.utcDefault

grabFile :: String -> IO (L.Filename, C.FileContents)
grabFile s = fmap f (TIO.readFile s)
  where
    f c = (L.Filename . X.pack $ s, C.FileContents c)

renderLedger :: C.Ledger -> X.Text
renderLedger (C.Ledger ls) =
  let renderer = I.render defaultTimeZone groupingSpec radGroup
  in fromMaybe (error "error when rendering ledger.")
               (fmap X.concat . mapM renderer . map snd $ ls)

grabFiles :: IO C.Ledger
grabFiles = do
  as <- getArgs
  ps <- mapM grabFile as
  let parsed = C.parse defaultTimeZone radGroup ps
  Ex.switch (error . show) return parsed


main :: IO ()
main = do
  l <- grabFiles
  let r = renderLedger l
  TIO.putStr r
