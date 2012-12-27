module Penny.Zinc.Parser.Defaults where

import qualified Text.Matchers.Text as M
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Data.Text as X
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Scheme.Dark as Dark

data T =
  T { sensitive :: M.CaseSensitive
    , factory :: M.CaseSensitive -> X.Text
                 -> Ex.Exceptional X.Text (X.Text -> Bool)
    , currentTime :: L.DateTime
    , colorToFile :: ColorToFile
    , scheme :: E.Scheme
    }

defaultFromRuntime
  :: S.Runtime
  -> T
defaultFromRuntime rt =
  T { sensitive = M.Insensitive
    , factory = (\c t -> return (M.within c t))
    , currentTime = S.currentTime rt
    , colorToFile = ColorToFile False
    , scheme = Dark.scheme
    }

-- | Whether to use color when standard output is not a terminal.
newtype ColorToFile = ColorToFile { unColorToFile :: Bool }
  deriving (Eq, Show)

