module Penny.Brass.Util where

import qualified Data.ByteString.Lazy as BS
import Data.Word (Word8)

newtype Input = Input { unInput :: BS.ByteString }

type AlexInput = Input

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (Input _ bs) =
  case BS.uncons bs of
    Nothing -> Nothing
    Just (w8, leftover) -> Just (w8, Input leftover)
