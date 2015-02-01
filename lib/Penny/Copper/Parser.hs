{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,
             MultiParamTypeClasses #-}
module Penny.Copper.Parser where

import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Core hiding (Zero)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fdbl

-- | The 'LineColPos' that ships with uu-parsinglib is not an instance
-- of 'Eq'; having an 'Eq' instance can help enormously with testing
data LineColPosA = LineColPosA !Int !Int !Int
  deriving (Eq, Ord, Show)

instance IsLocationUpdatedBy LineColPosA Char where
  advance (LineColPosA lin ps ab) c = case c of
    '\n' -> LineColPosA (lin + 1) 0 (ab + 1)
    '\t' -> LineColPosA lin (ps + 8 - (ps - 1) `mod` 8) (ab + 1)
    _ -> LineColPosA lin (ps + 1) (ab + 1)

type Parser = P (Str Char String LineColPosA)

pSeq :: Parser a -> Parser (Seq a)
pSeq p = fmap Seq.fromList $ many p

rSeq :: (a -> ShowS) -> Seq a -> ShowS
rSeq f = Fdbl.foldr (.) id . fmap f

rList :: (a -> ShowS) -> [a] -> ShowS
rList f = foldr (.) id . fmap f

