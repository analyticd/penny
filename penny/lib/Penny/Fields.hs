{-# LANGUAGE TemplateHaskell #-}
module Penny.Fields where

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (ZonedTime)

data TopLineFields = TopLineFields
  { _date :: ZonedTime
  , _payee :: Maybe Text
  } deriving Show

Lens.makeLenses ''TopLineFields

data PostingFields = PostingFields
  { _number :: Maybe Integer
  , _flag :: Maybe Text
  , _account :: Seq Text
  , _fitid :: Maybe Text
  , _tags :: Seq Text
  } deriving Show

Lens.makeLenses ''PostingFields
