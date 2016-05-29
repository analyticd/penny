{-# LANGUAGE TemplateHaskell #-}
module Penny.Fields where

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (ZonedTime)

data TopLineFields = TopLineFields
  { _date :: ZonedTime
  , _payee :: Maybe Text
  } deriving Show

Lens.makeLenses ''TopLineFields

emptyTopLineFields :: ZonedTime -> TopLineFields
emptyTopLineFields zt = TopLineFields zt Nothing

data PostingFields = PostingFields
  { _number :: Maybe Integer
  , _flag :: Maybe Text
  , _account :: Seq Text
  , _fitid :: Maybe Text
  , _tags :: Seq Text
  } deriving Show

emptyPostingFields :: PostingFields
emptyPostingFields = PostingFields Nothing Nothing (Seq.empty) Nothing
  Seq.empty

Lens.makeLenses ''PostingFields
