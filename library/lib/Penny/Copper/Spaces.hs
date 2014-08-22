module Penny.Copper.Spaces where

import Control.Monad
import Penny.Numbers.Natural
import qualified Penny.Numbers.Natural as N
import Text.Parsec hiding (parse)
import qualified Data.Text as X
import Penny.Copper.Render
import Data.Monoid

newtype Spaces = Spaces { unSpaces :: Pos }
  deriving (Eq, Ord, Show)

instance Renderable Spaces where
  render (Spaces p)
    | ip > fromIntegral (maxBound :: Int) =
        error "render spaces: too many spaces!"
    | otherwise = X.replicate (fromIntegral ip) (X.singleton ' ')
    where
      ip = unPos p
  parser = do
    c <- many1 (char ' ')
    case nonNegToPos $ N.length c of
      Nothing -> error "spaces: parse: error"
      Just p -> return $ Spaces p

data PreSpace a = PreSpace
  { psSpaces :: Spaces
  , psData :: a
  } deriving (Eq, Ord, Show)

instance Functor PreSpace where
  fmap f (PreSpace s a) = PreSpace s (f a)

instance Renderable a => Renderable (PreSpace a) where
  render (PreSpace s d) = render s <> render d
  parser = liftM2 PreSpace parser parser
