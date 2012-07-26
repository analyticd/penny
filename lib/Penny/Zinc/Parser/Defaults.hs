module Penny.Zinc.Parser.Defaults where

import qualified Text.Matchers.Text as M
import qualified Penny.Lincoln as L
import qualified Penny.Copper as Cop
import qualified Penny.Shield as S
import qualified Data.Text as X
import qualified Control.Monad.Exception.Synchronous as Ex

data T =
  T { sensitive :: M.CaseSensitive
    , factory :: M.CaseSensitive -> X.Text
                 -> Ex.Exceptional X.Text (X.Text -> Bool)
    , currentTime :: L.DateTime
    , defaultTimeZone :: Cop.DefaultTimeZone
    , radGroup :: Cop.RadGroup }

defaultFromRuntime ::
  Cop.DefaultTimeZone
  -> Cop.RadGroup
  -> S.Runtime
  -> T
defaultFromRuntime dtz rg rt =
  T { sensitive = M.Insensitive
    , factory = (\c t -> return (M.within c t))
    , currentTime = S.currentTime rt
    , defaultTimeZone = dtz
    , radGroup = rg }
  
