-- | penny-gibberish generates a valid ledger file that is full of
-- junk. The first argument is the size parameter for the
-- generator. The second parameter is a seed for the generator.

module Main where

import Data.Maybe (fromMaybe)
import qualified PennyTest.Penny.Copper.Gen.Parsers as GP
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Test.QuickCheck.Gen as G
import System.Random (mkStdGen)
import qualified Penny.Copper.Render as R
import System.Environment (getArgs)

groupSpecs :: R.GroupSpecs
groupSpecs = R.GroupSpecs R.NoGrouping R.NoGrouping

-- | Generates the gibberish text.
mkGibberish
  :: Int
  -- ^ Size parameter

  -> Int
  -- ^ Generator seed

  -> X.Text

mkGibberish s g =
  fromMaybe (error "could not render ledger.")
  . R.ledger groupSpecs
  . Ex.resolve (const (error "Could not generate ledger."))
  . (\gen -> G.unGen gen (mkStdGen g) s)
  . Ex.runExceptionalT
  . fmap fst
  $ GP.ledger

main :: IO ()
main = do
  s:g:[] <- getArgs
  TIO.putStr $ mkGibberish (read s) (read g)
