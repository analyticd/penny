module Penny.Copper.Convert.TopLine where

import Data.Sequence
import Data.Foldable (foldlM)

-- Tree imports, qualified with T
import qualified Penny.Copper.Tree.TopLine as T
import qualified Penny.Copper.Tree.Date as T
import qualified Penny.Copper.Tree.PostSpace as T

-- Fields imports, qualified with F
import qualified Penny.Copper.Convert.Fields as F

-- Main Penny imports, qualified with C
import qualified Penny.TopLine as C
import qualified Penny.Common as C
import qualified Penny.Serial as C

data Error
  = AlreadyDate
  | AlreadyTime
  | AlreadyFlag
  | AlreadyNumber
  | DateError F.DateError
  | NoDate
  deriving (Eq, Ord, Show)

type MkTopLine
  = C.Clxn
  -> C.Serial
  -- ^ Global serial
  -> C.Serial
  -- ^ Collection serial
  -> C.Location
  -> C.Memo
  -> C.TopLine

topLine
  :: T.TopLine
  -> Either Error MkTopLine
topLine (T.TopLine sq mp _) = do
  sc <- scanItems sq
  dy <- maybe (Left NoDate) Right . scDate $ sc
  ti <- maybe (return Nothing) (Right . Just) . scTime $ sc
  dt <- either (Left . DateError) Right
    $ F.dateTime dy ti
  let pye = fmap F.topLinePayeeToPayee mp
  return $ \clxn glbl cs loc mem ->
    C.TopLine { C.tlDateTime = dt
              , C.tlMemo = mem
              , C.tlNumber = scNumber sc
              , C.tlFlag = scFlag sc
              , C.tlPayee = pye
              , C.tlLocation = loc
              , C.tlClxn = clxn
              , C.tlGlobalSer = glbl
              , C.tlClxnSer = cs
              }

data Scan = Scan
  { scDate :: Maybe T.Date
  , scTime :: Maybe T.Time
  , scFlag :: Maybe C.Flag
  , scNumber :: Maybe C.Number
  } deriving (Eq, Ord, Show)

emptyScan :: Scan
emptyScan = Scan Nothing Nothing Nothing Nothing

scanItem :: Scan -> T.Item -> Either Error Scan
scanItem s i = case i of
  T.I0 (T.PostSpace d _) -> maybe (Right s { scDate = Just d })
    (const $ Left AlreadyDate) (scDate s)

  T.I1 (T.PostSpace t _) -> maybe (Right s { scTime = Just t })
    (const $ Left AlreadyTime) (scTime s)

  T.I2 (T.PostSpace f _) -> maybe
    (Right s { scFlag = Just (F.toFlag f) })
    (const $ Left AlreadyFlag) (scFlag s)

  T.I3 (T.PostSpace n _) -> maybe
    (Right s { scNumber = Just (F.toNumber n) })
    (const $ Left AlreadyNumber) (scNumber s)

scanItems :: Seq T.Item -> Either Error Scan
scanItems = foldlM scanItem emptyScan


