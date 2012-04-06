module Penny.Brass.Start where

import qualified Data.Text as X

import Penny.Lincoln.Strict (List)

data Comment = Comment !(List CommentContent)

data CommentContent = CommentText !X.Text

exclamation :: X.Text
exclamation = X.singleton '!'

spaces :: Int -> X.Text
spaces i = X.replicate i (X.singleton ' ')

