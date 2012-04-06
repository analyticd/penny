module Penny.Brass.Start where

import qualified Data.Text as X

import Penny.Lincoln.Strict (List)

data FileItem = ItemComment Comment
                deriving Show

data Comment = Comment !(List CommentContent)
               deriving (Show, Eq)

data CommentContent = CommentText !X.Text
                      deriving (Show, Eq)
exclamation :: X.Text
exclamation = X.singleton '!'

spaces :: Int -> X.Text
spaces i = X.replicate i (X.singleton ' ')



period :: X.Text
period = X.singleton '.'

