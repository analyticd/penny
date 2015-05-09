{-# LANGUAGE OverloadedStrings #-}

module Penny.Semantic.Matcher where

import Data.Monoid
import Penny.Matcher
import Penny.Display
import Penny.Semantic
import Data.String

equal
  :: (SemanticEq s, Display s, Monad m)
  => s
  -> Matcher s m s
equal tgt = do
  subj <- getSubject
  let subjStr = fromString $ display subj ""
      tgtStr = fromString $ display tgt ""
  if semanticEq subj tgt
    then proclaim (subjStr <> " is equal to " <> tgtStr) >> accept subj
    else proclaim (subjStr <> " is not equal to " <> tgtStr) >> reject


greater
  :: (SemanticOrd s, Display s, Monad m)
  => s
  -> Matcher s m s
greater tgt = do
  subj <- getSubject
  let subjStr = fromString $ display subj ""
      tgtStr = fromString $ display tgt ""
  if semanticOrd subj tgt == GT
    then proclaim (subjStr <> " is greater than " <> tgtStr) >> accept subj
    else proclaim (subjStr <> " is not greater than " <> tgtStr) >> reject

less
  :: (SemanticOrd s, Display s, Monad m)
  => s
  -> Matcher s m s
less tgt = do
  subj <- getSubject
  let subjStr = fromString $ display subj ""
      tgtStr = fromString $ display tgt ""
  if semanticOrd subj tgt == LT
    then proclaim (subjStr <> " is less than " <> tgtStr) >> accept subj
    else proclaim (subjStr <> " is not less than " <> tgtStr) >> reject
