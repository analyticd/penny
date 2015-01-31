{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,
             MultiParamTypeClasses #-}
module Penny.Copper.Classes where

import Control.Applicative
import Data.Sequence (Seq)
import Penny.Lincoln.Rep
import Penny.Lincoln.Rep.Digits
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin
import Penny.Copper.LincolnTypes
import Penny.Copper.Parser

-- | Things that can be parsed.

class Parseable a where
  parser :: Parser a


-- | Things that can be parsed, but they must be passed a parser for
-- the radix point.
class ParseableR a where
  parserR :: Parser (Radix r) -> Parser (a r)

-- | Things that can be parsed, but they must be passed a parser for a
-- grouping character.

class ParseableG a where
  parserG :: Parser g -> Parser (a g)

-- | Things that can be parsed, but they must be passed a parser for a
-- grouping character and a parser for the radix point.

class ParseableRG a where
  parserRG :: Parser (Radix g) -> Parser g -> Parser (a g)

instance Parseable Novem where parser = pNovem
instance Renderable Novem where render = rNovem

instance Parseable Decem where parser = pDecem
instance Renderable Decem where render = rDecem

instance Parseable Grouper where parser = pGrouper
instance Renderable Grouper where render = rGrouper

instance Parseable RadCom where parser = pRadCom
instance Renderable RadCom where render = rRadCom

instance Parseable (Radix RadCom) where parser = pRadixRadCom
instance Renderable (Radix RadCom) where render = rRadixRadCom

instance Parseable RadPer where parser = pRadPer
instance Renderable RadPer where render = rRadPer

instance Parseable (Radix RadPer) where parser = pRadixRadPer
instance Renderable (Radix RadPer) where render = rRadixRadPer

instance Parseable r => Parseable (Seq r) where parser = pSeq parser
instance Renderable r => Renderable (Seq r) where render = rSeq render

instance Parseable Side where parser = pSide
instance Renderable Side where render = rSide

instance Parseable Zero where parser = pZero
instance Renderable Zero where render = rZero

instance ParseableRG NilGrouped where parserRG = pNilGrouped
instance RenderableRG NilGrouped where renderRG = rNilGrouped

instance ParseableR NilUngrouped where parserR = pNilUngrouped
instance RenderableR NilUngrouped where renderR = rNilUngrouped

instance ParseableG BG7 where parserG = pBG7
instance RenderableG BG7 where renderG = rBG7

instance ParseableG BG8 where parserG = pBG8
instance RenderableG BG8 where renderG = rBG8

instance ParseableG BG6 where parserG = pBG6
instance RenderableG BG6 where renderG = rBG6

instance ParseableG BG5 where parserG = pBG5
instance RenderableG BG5 where renderG = rBG5

instance ParseableRG BG1 where parserRG = pBG1
instance RenderableRG BG1 where renderRG = rBG1

instance ParseableR BrimUngrouped where parserR = pBrimUngrouped
instance RenderableR BrimUngrouped where renderR = rBrimUngrouped

instance ParseableRG BrimGrouped where parserRG = pBrimGrouped
instance RenderableRG BrimGrouped where renderRG = rBrimGrouped

instance ParseableRG Brim where parserRG = pBrim
instance RenderableRG Brim where renderRG = rBrim

instance ParseableRG Nil where parserRG = pNil
instance RenderableRG Nil where renderRG = rNil

instance ParseableRG NilOrBrimScalar where parserRG = pNilOrBrimScalar
instance RenderableRG NilOrBrimScalar where renderRG = rNilOrBrimScalar

instance Parseable PluMin where parser = pPluMin
instance Renderable PluMin where render = rPluMin

instance Parseable a => Parseable [a] where parser = many parser
instance Renderable a => Renderable [a] where
  render = foldr (.) id . fmap render

-- | Things that can be rendered.

class Renderable a where
  render :: a -> ShowS

-- | Things that can be rendered, but they must be passed a renderer
-- for the radix point.
class RenderableR a where
  renderR :: (Radix r -> ShowS) -> a r -> ShowS

-- | Things that can be rendered, but they must be passed a renderer
-- for a grouping character.

class RenderableG a where
  renderG :: (r -> ShowS) -> a r -> ShowS

-- | Things that can be rendered, but they must be passed renderers
-- for a grouping character and a radix point.
class RenderableRG a where
  renderRG :: (Radix r -> ShowS) -> (r -> ShowS) -> a r -> ShowS

