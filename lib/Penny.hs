{-# LANGUAGE ScopedTypeVariables #-}
-- | Main entry into the Penny REPL interface.

module Penny
  (

  -- * Primitive types
    Unsigned
  , Commodity
  -- * Clatcher
  , Clatcher

  -- * Commands
  , preload
  , open
  , convert
  , pre
  , sort
  , post
  , out
  , report
  ) where

import Penny.Amount
import Penny.Commodity
import Penny.Converter
import Control.Lens
  ( (.~), (&)
  , Lens'
  )
import qualified Data.Sequence as Seq
import Penny.Clatch
import Penny.Clatcher (Clatcher)
import Penny.Natural
import Penny.Stream
import qualified Penny.Clatcher as C

preload :: Monoid r => String -> IO (Clatcher r C.LoadScroll)
preload s = fmap f (C.preload s)
  where
    f scroll = mempty & C.load .~ (Seq.singleton scroll)

open :: Monoid r => String -> Clatcher r C.LoadScroll
open s = mempty & C.load .~ (Seq.singleton (C.open s))

convert :: Monoid r => (Amount -> Maybe Amount) -> Clatcher r l
convert f = mempty & C.converter .~ Converter f

pre :: Monoid r => (Converted () -> Bool) -> Clatcher r l
pre f = mempty & C.pre .~ f

sort
  :: Monoid r
  => (Prefilt () -> Prefilt () -> Ordering)
  -> Clatcher r l
sort f = mempty & C.sort .~ f

post
  :: Monoid r
  => (Totaled () -> Bool)
  -> Clatcher r l
post f = mempty & C.post .~ f

out
  :: Monoid r
  => IO Stream
  -> Clatcher r l
out a = mempty & C.out .~ a

report
  :: Monoid r
  => r
  -> Clatcher r l
report r
  = (mempty :: Monoid r => Clatcher r l)
  & (C.report :: Lens' (Clatcher r l) r) .~ r
