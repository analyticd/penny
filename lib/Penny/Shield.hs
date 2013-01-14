-- | Shield - the Penny runtime environment
--
-- Both Cabin and Copper can benefit from knowing information about
-- the Penny runtime environment, such as environment variables and
-- whether standard output is a terminal. That information is provided
-- by the Runtime type. In the future this module may also provide
-- information about the POSIX locale configuration. For now, that
-- information would require reaching into the FFI and so it is not
-- implemented.

module Penny.Shield (
  ScreenLines,
  unScreenLines,
  ScreenWidth,
  unScreenWidth,
  Output(IsTTY, NotTTY),
  Term,
  unTerm,
  Runtime,
  environment,
  currentTime,
  output,
  screenLines,
  screenWidth,
  term,
  runtime)
  where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Time as T
import System.Environment (getEnvironment)
import System.IO (hIsTerminalDevice, stdout)

import qualified Penny.Lincoln.Bits as B

data ScreenLines = ScreenLines { unScreenLines :: Int }
                 deriving Show

newtype ScreenWidth = ScreenWidth { unScreenWidth :: Int }
                      deriving Show

data Output = IsTTY | NotTTY deriving (Eq, Ord, Show)

newtype Term = Term { unTerm :: String } deriving Show

-- | Information about the runtime environment.
data Runtime = Runtime { environment :: [(String, String)]
                       , currentTime :: B.DateTime
                       , output :: Output }

runtime :: IO Runtime
runtime = Runtime
          <$> getEnvironment
          <*> (toDT <$> T.getZonedTime)
          <*> findOutput
          where
            toDT t = case B.fromZonedTime t of
              Nothing -> error "time conversion error"
              Just ti -> ti

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
