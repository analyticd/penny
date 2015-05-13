{-# LANGUAGE OverloadedStrings #-}
module Penny.PluMin.Matcher where

import Penny.Matcher
import Penny.PluMin

plus :: Monad m => Matcher PluMin m ()
plus = getSubject >>= f
  where
    f s | s == Plus = proclaim "is Plus" >> accept ()
        | otherwise = proclaim "is Minus" >> reject


minus :: Monad m => Matcher PluMin m ()
minus = getSubject >>= f
  where
    f s | s == Plus = proclaim "is Plus" >> reject
        | otherwise = proclaim "is Minus" >> accept ()

