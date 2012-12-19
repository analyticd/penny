module Main where

import qualified Types as OY
import qualified Penny.Brenner.Types as Y
import qualified Penny.Brenner.Util as U
import qualified Data.Text as X
import Data.Maybe (fromJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  fOld:fNew:[] <- getArgs
  old <- fmap read $ readFile fOld
  let new = map toNewItem old
  U.saveDb (Y.DbLocation . X.pack $ fNew) new

toNewItem :: (OY.UNumber, OY.Posting) -> (Y.UNumber, Y.Posting)
toNewItem (OY.UNumber ou, p) = (Y.UNumber ou, p')
  where
    p' = Y.Posting
      { Y.date = Y.Date (OY.unDate . OY.date $ p)
      , Y.desc = Y.Desc (X.pack . OY.unDesc . OY.desc $ p)
      , Y.incDec =
          case OY.incDec p of
            OY.Increase -> Y.Increase
            _ -> Y.Decrease
      , Y.amount = fromJust . Y.mkAmount . OY.unAmount
                   . OY.amount $ p
      , Y.payee = Y.Payee (X.pack . OY.unPayee . OY.payee $ p)
      , Y.fitId = Y.FitId (X.pack . OY.unAmexId . OY.amexId $ p)
      }
