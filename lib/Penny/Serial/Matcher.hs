{-# LANGUAGE RankNTypes #-}
module Penny.Serial.Matcher where

import Control.Lens
import Penny.Matcher
import Penny.Natural
import qualified Penny.Serial as S
import Penny.Serial (Serial, Serset)

serial :: Monad m => Matcher Unsigned m a -> Matcher Serial m a
serial = nest _Wrapped

forward :: Monad m => Matcher Serial m a -> Matcher S.Forward m a
forward = nest _Wrapped

backward :: Monad m => Matcher Serial m a -> Matcher S.Backward m a
backward = nest _Wrapped

serset :: Monad m => Matcher Serset m a -> Matcher (S.Sersetted b) m a
serset = nest S.serset

sersetee :: Monad m => Matcher a m r -> Matcher (S.Sersetted a) m r
sersetee = nest S.sersetee
