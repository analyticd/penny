module Penny.Brass.Start where

import qualified Data.Text as X

import Penny.Lincoln.Strict (List)

data Comment = Comment !(List CommentContent)

data CommentContent = CommentText !X.Text


