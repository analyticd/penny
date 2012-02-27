-- | Shield - the Penny runtime environment
--
-- Both Cabin and Copper can benefit from knowing information about
-- the Penny runtime environment, such as environment variables and
-- whether standard output is a terminal. That information is provided
-- by the Runtime type. In the future this module may also provide
-- information about the POSIX locale configuration. For now, that
-- information would require reaching into the FFI and so it is not
-- implemented.

module Penny.Shield where

import Control.Applicative ((<$>), (<*>))
import Data.Time (getCurrentTime)
import System.Environment (getEnvironment)
import System.IO (hIsTerminalDevice, stdout)

import Penny.Lincoln.Bits (DateTime(DateTime))

data ScreenLines = ScreenLines { unLines :: Int }
                 deriving Show

newtype ScreenWidth = ScreenWidth { unScreenWidth :: Int }
                      deriving Show

data Output = IsTTY | NotTTY

newtype Term = Term { unTerm :: String } deriving Show

-- | Information about the runtime environment.
data Runtime = Runtime { environment :: [(String, String)]
                       , currentTime :: DateTime
                       , output :: Output }

runtime :: IO Runtime
runtime = Runtime
          <$> getEnvironment
          <*> (DateTime <$> getCurrentTime)
          <*> findOutput

findOutput :: IO Output
findOutput = do
  isTerm <- hIsTerminalDevice stdout
  return $ if isTerm then IsTTY else NotTTY

screenLines :: Runtime -> Maybe ScreenLines
screenLines r = 
  (lookup "LINES" . environment $ r)
  >>= safeRead
  >>= return . ScreenLines

screenWidth :: Runtime -> Maybe ScreenWidth
screenWidth r =
  (lookup "COLUMNS" . environment $ r)
  >>= safeRead
  >>= return . ScreenWidth

term :: Runtime -> Maybe Term
term r =
  (lookup "TERM" . environment $ r)
  >>= return . Term

-- | Read, but without crashes.
safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
  (a, []):[] -> Just a
  _ -> Nothing
