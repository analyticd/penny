{-# LANGUAGE OverloadedStrings #-}
module Penny.PluMin.Matcher where

import Control.Applicative
import Penny.Matcher
import Penny.PluMin

plus :: Monad m => Matcher PluMin m ()
plus = getSubject >>= f
  where
    f s | s == Plus = return ()
        | otherwise = empty


minus :: Monad m => Matcher PluMin m ()
minus = getSubject >>= f
  where
    f s | s == Plus = return ()
        | otherwise = empty

