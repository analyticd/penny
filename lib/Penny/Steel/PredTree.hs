{-# LANGUAGE OverloadedStrings #-}

-- | Trees of predicates.
--
-- Exports 'not', which of course conflicts with the Prelude function
-- of the same name, so you probably want to import this module
-- qualified.

module Penny.Steel.PredTree
  ( Pdct(..)
  , always
  , never
  , namedAnd
  , namedOr
  , namedNot
  , not
  , operand
  , neverFalse
  , neverTrue
  , (&&&)
  , (|||)
  , Level
  , IndentAmt
  , ShowDiscards
  , showPdct
  , rename
  , eval
  , evaluate
  , boxPdct
  , boxNode
  ) where

import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat, mempty)
import qualified Penny.Steel.Chunk as C
import qualified Penny.Steel.Chunk.Switch as Sw
import Prelude hiding (not)
import qualified Prelude

type Label = Text

-- | A tree of predicates.
data Pdct a = Pdct Label (Node a)

instance Show (Pdct a) where
  show _ = "predicate"

-- | Renames the top level of the Pdct. The function you pass will be
-- applied to the old name.
rename :: (Text -> Text) -> Pdct a -> Pdct a
rename f (Pdct l n) = Pdct (f l) n

data Node a
  = And [Pdct a]
  -- ^ None of the Pdct in list may be Just False. An empty list or
  -- list with only Nothing is Just True.

  | Or [Pdct a]
  -- ^ At least one of the Pdct in the list must be Just True. An
  -- empty lis or list with only Nothing is Just False.

  | Not (Pdct a)
  -- ^ Just True is Just False and vice versa; Nothing remains Nothing.

  | Operand (a -> Maybe Bool)

-- | Given a function that un-boxes values of type b, changes a Node
-- from type a to type b.
boxNode
  :: (b -> a)
  -> Node a
  -> Node b
boxNode f n = case n of
  And ls -> And $ map (boxPdct f) ls
  Or ls -> Or $ map (boxPdct f) ls
  Not o -> Not $ boxPdct f o
  Operand g -> Operand $ \b -> g (f b)

-- | Given a function that un-boxes values of type b, changes a Pdct
-- from type a to type b.
boxPdct
  :: (b -> a)
  -> Pdct a
  -> Pdct b
boxPdct f (Pdct l n) = Pdct l $ boxNode f n

namedAnd :: Text -> [Pdct a] -> Pdct a
namedAnd t = Pdct t . And

namedOr :: Text -> [Pdct a] -> Pdct a
namedOr t = Pdct t . Or

namedNot :: Text -> Pdct a -> Pdct a
namedNot t = Pdct t . Not

not :: Pdct a -> Pdct a
not = namedNot (X.pack "not")

-- | Creates a new operand. The Pdct is Just True or Just False, never
-- Nothing.
operand :: Text -> (a -> Bool) -> Pdct a
operand t = Pdct t . Operand . fmap Just

-- | Turns an existing Pdct to one that never says False. If the
-- underlying predicate returns Just True, the new Pdct also returns
-- Just True. Otherwise, the Pdct returns Nothing.  Has no effect on
-- non-Operand Pdct.
neverFalse :: Pdct a -> Pdct a
neverFalse p@(Pdct l n) = case n of
  Operand f ->
    let f' a = case f a of
          Nothing -> Nothing
          Just b -> if b then Just True else Nothing
    in Pdct l (Operand f')
  _ -> p

-- | Turns an existing Pdct to one that never says True. If the
-- underlying predicate returns Just False, the new Pdct also returns
-- Just False. Otherwise, the Pdct returns Nothing.  Has no effect on
-- non-Operand Pdct.
neverTrue :: Pdct a -> Pdct a
neverTrue p@(Pdct l n) = case n of
  Operand f ->
    let f' a = case f a of
          Nothing -> Nothing
          Just b -> if Prelude.not b then Just False else Nothing
    in Pdct l (Operand f')
  _ -> p


-- | Returns a tree that is always True.
always :: Pdct a
always = Pdct "always True" (Operand (const (Just True)))

-- | Returns a tree that is always False.
never :: Pdct a
never = Pdct "always False" (Operand (const (Just False)))

(&&&) :: Pdct a -> Pdct a -> Pdct a
(&&&) x y = Pdct "and" (And [x, y])
infixr 3 &&&

(|||) :: Pdct a -> Pdct a -> Pdct a
(|||) x y = Pdct "or" (Or [x, y])
infixr 2 |||

type Level = Int
type IndentAmt = Int

-- | Indents text, and adds a newline to the end.
indent :: IndentAmt -> Level -> [C.Chunk] -> [C.Chunk]
indent amt lvl cs = idt : (cs ++ [nl])
  where
    idt = C.chunk C.defaultTextSpec
                  (X.replicate (lvl * amt) " ")
    nl = C.chunk C.defaultTextSpec (X.singleton '\n')

defaultChunk :: Text -> C.Chunk
defaultChunk = C.chunk C.defaultTextSpec

showPdct :: IndentAmt -> Level -> Pdct a -> [C.Chunk]
showPdct amt lvl (Pdct l pd) = case pd of
  And ls -> indent amt lvl [defaultChunk l]
            <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Or ls -> indent amt lvl [defaultChunk l]
           <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Not t -> indent amt lvl [defaultChunk l]
           <> showPdct amt (lvl + 1) t
  Operand _ -> indent amt lvl [defaultChunk l]


labelBool :: Text -> Maybe Bool -> [C.Chunk]
labelBool t b = [open, trueFalse, close, blank, txt]
  where
    trueFalse = C.chunk ts tf
    (tf, ts) = case b of
      Nothing -> ("discard", Sw.switchForeground C.color8_f_yellow
                             C.color256_f_3 C.defaultTextSpec)
      Just bl -> if bl
        then ("TRUE", Sw.switchForeground C.color8_f_green
                      C.color256_f_2 C.defaultTextSpec)
        else ("FALSE", Sw.switchForeground C.color8_f_red
                       C.color256_f_1 C.defaultTextSpec)
    open = C.chunk C.defaultTextSpec "["
    close = C.chunk C.defaultTextSpec "]"
    blank = C.chunk C.defaultTextSpec (X.replicate blankLen " ")
    blankLen = X.length "discard" - X.length tf + 1
    txt = C.chunk C.defaultTextSpec t

type ShowDiscards = Bool

-- | Evaluates a Pdct.
eval :: Pdct a -> a -> Maybe Bool
eval (Pdct _ n) a = case n of
  And ps -> Just . and . catMaybes $ [flip eval a] <*> ps
  Or ps -> Just . or . catMaybes $ [flip eval a] <*> ps
  Not p -> fmap Prelude.not $ eval p a
  Operand f -> f a

-- | Verbosely evaluates a Pdct.
evaluate
  :: IndentAmt
  -- ^ Indent each level by this many spaces.

  -> ShowDiscards
  -- ^ If True, show discarded test results; otherwise, hide
  -- them.

  -> a
  -- ^ The subject to evaluate

  -> Level
  -- ^ How many levels deep in the tree we are. Start at level 0. This
  -- determines the level of indentation.
  -> Pdct a
  -> (Maybe Bool, [C.Chunk])
evaluate i sd a lvl (Pdct l pd) = case pd of

  And ps -> let (resBool, resTxt) = evalAnd i sd a (lvl + 1) ps
                txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
            in (Just resBool, txt)

  Or ps -> let (resBool, resTxt) = evalOr i sd a (lvl + 1) ps
               txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
           in (Just resBool, txt)

  Not p -> let (childMayBool, childTxt) = evaluate i sd a (lvl + 1) p
               thisMayBool = fmap Prelude.not childMayBool
               thisTxt = indent i lvl (labelBool l thisMayBool)
               txt = if sd || isJust thisMayBool
                     then thisTxt <> childTxt else mempty
           in (thisMayBool, txt)

  Operand p -> let res = p a
                   txt = indent i lvl (labelBool l res)
               in (res, if sd || isJust res then txt else mempty)

evalAnd :: IndentAmt -> ShowDiscards -> a
        -> Level -> [Pdct a] -> (Bool, [C.Chunk])
evalAnd i sd a l ts = (Prelude.not foundFalse, txt)
  where
    (foundFalse, txt) = go ts (False, mempty)
    go [] p = p
    go (x:xs) (fndFalse, acc) =
      if fndFalse
      then (fndFalse, acc <> indent i l
                             [defaultChunk "(short circuit)"])
      else let (res, cTxt) = evaluate i sd a l x
               fndFalse' = maybe False Prelude.not res
           in go xs (fndFalse', acc <> cTxt)

evalOr :: IndentAmt -> ShowDiscards -> a
       -> Level -> [Pdct a] -> (Bool, [C.Chunk])
evalOr i sd a l ts = (foundTrue, txt)
  where
    (foundTrue, txt) = go ts (False, mempty)
    go [] p = p
    go (x:xs) (fnd, acc) =
      if fnd
      then (fnd, acc <> indent i l
                        [defaultChunk "(short circuit)"])
      else let (res, cTxt) = evaluate i sd a l x
               fnd' = fromMaybe False res
           in go xs (fnd', acc <> cTxt)

