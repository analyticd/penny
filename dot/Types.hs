{-# LANGUAGE RecursiveDo #-}
module Types where

import Dot
import Data.List (intersperse)
import Prelude hiding
  ( maybe
  , product
  , either
  , seq
  )

-- | Data constructor.
dacon
  :: String
  -- ^ Name for this data constructor.
  -> [Tycon]
  -- ^ Fields of the data constructor.
  -> Dot NodeId
dacon nm fs = do
  nid <- node [("label", nm), ("shape", "box")]
  let mkEdge dest = edge nid (tyNodeId dest) []
  mapM_ mkEdge fs
  return nid

data Typename = Typename
  { tyId :: String
  , tyParam :: [Typename]
  } deriving (Eq, Ord, Show)

-- | Type with no parameterized types.
kind1 :: String -> Typename
kind1 s = Typename s []

typenameToString :: Typename -> String
typenameToString (Typename s ts)
  = concat
  . intersperse " "
  . (s:)
  . map (\n -> "(" ++ n ++ ")")
  . map typenameToString
  $ ts

data Tycon = Tycon
  { tyName :: Typename
  , tyNodeId :: NodeId
  } deriving Show


-- | Builds an entire type.
tycon
  :: Typename
  -> [(String, [Tycon])]
  -- ^ Name for each data constructor, and its associated fields.
  -> Dot Tycon
tycon nm fs = do
  nid <- node [("label", typenameToString nm), ("shape", "oval")]
  dacons <- mapM (uncurry dacon) fs
  let mkEdge dest = edge nid dest [("style", "dotted")]
  mapM_ mkEdge dacons
  return $ Tycon nm nid

-- | A type with one empty constructor.
nullary
  :: Typename
  -> Dot Tycon
nullary s = tycon s [(tyId s, [])]

-- | an entire type with just one constructor; it will have the same
-- name as the type.  A newtype.
oneConst
  :: Typename
  -> Tycon
  -- ^ The contained type.
  -> Dot Tycon
oneConst n ty = tycon n [(tyId n, [ty])]

-- | Product types.  The constructor will have the same name as the type.
product
  :: Typename
  -> [Tycon]
  -- ^ Name for each field
  -> Dot Tycon
product n fs = tycon n [(tyId n, fs)]

-- | A type with no constructors.
opaque
  :: Typename
  -> Dot Tycon
opaque n = tycon n []

-- | An empty data constructor.  The single argument is the name of
-- the constructor.
empty :: String -> (String, [a])
empty s = (s, [])

-- | Labels for tuples.
tuplabel
  :: [Typename]
  -> Typename
tuplabel ls =
  Typename ( "(" ++ (concat . intersperse ", " . map typenameToString $ ls)
             ++ ")") []

tuple2
  :: Tycon
  -> Tycon
  -> Dot Tycon
tuple2 x y = product (tuplabel . map tyName $ [x, y]) [x, y]

-- | Builds a Typename that is parameterized on a given type.
copyTy
  :: String
  -- ^ Name of this type
  -> Tycon
  -- ^ Copy from this type
  -> Typename
copyTy n t = Typename n [tyName t]

-- | Builds a Typename that is parameterized on the same type as a
-- child Tycon.
copyUp
  :: String
  -- ^ Name of this type
  -> Tycon
  -- ^ Child type
  -> Typename
copyUp n = Typename n . tyParam . tyName

-- | Builds a Typename that is parameterized on the same type as a
-- child of a child Tycon.
copyUp2
  :: String
  -- ^ Name of this type
  -> Tycon
  -- ^ Child type
  -> Typename
copyUp2 n = Typename n . tyParam . get . tyParam . tyName
  where
    get ls = case ls of
      [] -> error $ "copyUp2 failed on typename " ++ n
      x:[] -> x

--
-- Primitives and non-Penny types
--

bool :: Dot Tycon
bool = tycon (kind1 "Bool") [empty "True", empty "False"]

either
  :: Tycon
  -- ^ Left
  -> Tycon
  -- ^ Right
  -> Dot Tycon
either l r = tycon (Typename "Either" (map tyName [l, r]))
  [("Left", [l]), ("Right", [r])]

int :: Dot Tycon
int = opaque (kind1 "Int")

maybe
  :: Tycon
  -- ^ Type constructor for the parameterized type
  -> Dot Tycon
maybe tyc = tycon (Typename "Maybe" [tyName tyc])
  [("Nothing", []), ("Just", [tyc])]

seq
  :: Typename
  -- ^ Name of parameterized type
  -> Dot Tycon
seq = opaque . Typename "Seq" . (:[])

string :: Dot Tycon
string = opaque (kind1 "String")

unit :: Dot Tycon
unit = nullary (kind1 "()")

--
-- Penny types
--

arithmeticError
  :: Tycon
  -- ^ String
  -> Dot Tycon
arithmeticError = oneConst (kind1 "ArithmeticError.T")

anna
  :: Tycon
  -- ^ Nil
  -> Tycon
  -- ^ Brim
  -> Dot Tycon
anna n b = tycon (copyUp "Anna.T" n) [("Nil", [n]), ("Brim", [b])]

arrangement
  :: Tycon
  -- ^ Orient
  -> Tycon
  -- ^ SpaceBetween
  -> Dot Tycon
arrangement o s = product (kind1 "Arrangement.T") [o, s]

brimUngrouped
  :: Tycon
  -- ^ Nodbu
  -> Tycon
  -- ^ BU2
  -> Dot Tycon
brimUngrouped n b = tycon (copyUp "BrimUngrouped.T" n)
  [ ("Masuno", [n]), ("Fracuno", [b])]


bg1
  :: Tycon
  -- ^ Grouper
  -> Tycon
  -- ^ DecDecsMayGroups
  -> Tycon
  -- ^ Maybe BG2
  -> Tycon
  -- ^ Radix
  -> Dot Tycon
bg1 g d b r = tycon (copyTy "BG1.T" g)
  [ ("GroupOnLeft", [g, d, b]), ("GroupOnRight", [r, d])]

bg2
  :: Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Maybe DecDecsMayGroups
  -> Dot Tycon
bg2 r d = product (copyUp "BG2.T" r) [r, d]


bg4
  :: Tycon
  -- ^ Maybe Zero
  -> Tycon
  -- ^ Radix
  -> Tycon
  -- ^ BG5
  -> Dot Tycon
bg4 z r b = product (copyUp "BG4.T" r) [z, r, b]

bg5
  :: Tycon
  -- ^ NovSeqDecsNE
  -> Tycon
  -- ^ Zeroes
  -> Tycon
  -- ^ BG6
  -> Dot Tycon
bg5 n z b = tycon (copyUp "BG5.T" n)
  [ ("Novem", [n]), ("Zeroes", [z, b]) ]

bg6
  :: Tycon
  -- ^ NovSeqDecsNE
  -> Tycon
  -- ^ Grouper
  -> Tycon
  -- ^ BG7
  -> Dot Tycon
bg6 n g b = tycon (copyUp "BG6.T" b)
  [("Novem", [n]), ("Group", [g, b])]


bg7
  :: Tycon
  -- ^ Zeroes
  -> Tycon
  -- ^ Grouping character
  -> Tycon
  -- ^ Nodecs3
  -> Dot Tycon
bg7 z g n = mdo
  tup <- tuple2 g res
  ei <- either tup n
  res <- tycon (Typename "BG7.T" [(tyName g)])
    [("LeadZeroes", [z, ei]), ("LeadNovem", [n])]
  return res

brim
  :: Tycon
  -- ^ BrimGrouped
  -> Tycon
  -- ^ BrimUngrouped
  -> Dot Tycon
brim g u = tycon (copyUp "Brim.T" g)
  [ ("Grouped", [g]), ("Ungrouped", [u])]

brimGrouped
  :: Tycon
  -- ^ NovDecs
  -> Tycon
  -- ^ BG1
  -> Tycon
  -- ^ BG4
  -> Dot Tycon
brimGrouped n b1 b4 = tycon (copyUp "BrimGrouped.T" b1)
  [("Masuno", [n, b1]), ("Fracuno", [b4])]

bu2
  :: Tycon
  -- ^ Zerabu
  -> Tycon
  -- ^ Radbu
  -> Dot Tycon
bu2 z r = tycon (copyUp "BU2.T" r)
  [ ("LeadingZero", [z]), ("NoLeadingZero", [r])]

bu3
  :: Tycon
  -- ^ Zenod
  -> Tycon
  -- ^ NovDecs
  -> Dot Tycon
bu3 z n = tycon (kind1 "BU3.T")
  [("Zeroes", [z]), ("NoZeroes", [n])]

decDecs
  :: Tycon
  -- ^ Decem
  -> Tycon
  -- ^ Decems
  -> Dot Tycon
decDecs d ds = product (kind1 "decDecs") [d, ds]

decDecsMayGroups
  :: Tycon
  -- ^ DecDecs
  -> Tycon
  -- ^ SeqDecs
  -> Dot Tycon
decDecsMayGroups d s = product (copyUp "DecDecsMayGroups.T" s) [d, s]

decem
  :: Tycon
  -- ^ Novem
  -> Dot Tycon
decem tyc = tycon (kind1 "Decem")
  [ empty "D0", ("Nonem", [tyc]) ]

decems :: Dot Tycon
decems = seq (kind1 "Decem")

decsGroup
  :: Tycon
  -- ^ Grouper
  -> Tycon
  -- ^ DecDecs
  -> Dot Tycon
decsGroup g d = product (copyTy "DecsGroup.T" g) [g,d]

grouper
  :: Tycon
  -- ^ Unique
  -> Dot Tycon
grouper un = tycon (Typename "Grouper.T" [tyName un])
  [ empty "Space"
  , empty "Thin"
  , empty "Under"
  , ("Unique", [un])
  ]

ng1
  :: Tycon
  -- ^ Radix.T
  -> Tycon
  -- ^ Zeroes.T
  -> Tycon
  -- ^ Grouper
  -> Tycon
  -- ^ Seq (ZGroup.T r)
  -> Dot Tycon
ng1 r z g s = product (copyUp "NG1.T" r) [r,z,g,z,s]


nil
  :: Tycon
  -- ^ NilUngrouped
  -> Tycon
  -- ^ NilGrouped
  -> Dot Tycon
nil nu ng = tycon (copyUp "Nil.T" nu)
  [ ("Ungrouped", [nu]), ("Grouped", [ng]) ]

nilGrouped
  :: Tycon
  -- ^ Zng
  -> Tycon
  -- ^ NG1
  -> Dot Tycon
nilGrouped zng ng1 = tycon (copyUp "NilGrouped.T" zng)
  [ ("LeadingZero", [zng]), ("NoLeadingZero", [ng1]) ]

nilUngrouped
  :: Tycon
  -- ^ Znu1
  -> Tycon
  -- ^ RadZ
  -> Dot Tycon
nilUngrouped z r = tycon (copyUp "NilUngrouped.T" r)
  [ ("NilUngrouped.LeadingZero", [z])
  , ("NilUngrouped.NoLeadingZero", [r])]

nodbu
  :: Tycon
  -- ^ NovDecs
  -> Tycon
  -- ^ Maybe Radem
  -> Dot Tycon
nodbu n r = product (copyUp "Nodbu.T" r) [n, r]

nodecs3
  :: Tycon
  -- ^ NovDecs
  -> Tycon
  -- ^ SeqDecs
  -> Dot Tycon
nodecs3 n s = product (copyUp2 "Nodecs3.T" s) [n, s]

nonZero :: Dot Tycon
nonZero = opaque (kind1 "NonZero.T")

novDecs
  :: Tycon
  -- ^ Novem
  -> Tycon
  -- ^ Decems
  -> Dot Tycon
novDecs n d = product (kind1 "NovDecs.T") [n, d]

novem :: Dot Tycon
novem = tycon (kind1 "Novem") . map f $ ([1..9] :: [Int])
  where
    f num = ('D' : show num, [])

novSeqDecsNE
  :: Tycon
  -- ^ NovDecs
  -> Tycon
  -- ^ SeqDecsNE
  -> Dot Tycon
novSeqDecsNE n s = product (copyUp "NovSeqDecsNE.T" s) [n, s]

orient :: Dot Tycon
orient
  = tycon (kind1 "Orient.T")
  . map empty
  . map ("Orient.CommodityOn" ++)
  $ ["Left", "Right"]

radbu
  :: Tycon
  -- ^ Radix
  -> Tycon
  -- ^ BU3
  -> Dot Tycon
radbu r b = product (copyUp "Radbu.T" r) [r, b]

radem
  :: Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Decems
  -> Dot Tycon
radem r d = product (copyUp "Radem.T" r) [r, d]

radix
  :: Typename
  -- ^ Name of parameterized type
  -> Dot Tycon
radix s = nullary (Typename "Radix.T" [s])


radun
  :: Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Maybe Zeroes
  -> Dot Tycon
radun r z = product (copyUp "Radun.T" r) [r, z]

radZ
  :: Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Zeroes
  -> Dot Tycon
radZ r z = product (copyUp "Radun.T" r) [r, z]

seqDecs
  :: Typename
  -- ^ Parameterized type
  -> Dot Tycon
seqDecs ty = seq (Typename "DecsGroup.T" [ty])

seqDecsNE
  :: Tycon
  -- ^ DecsGroup
  -> Tycon
  -- ^ SeqDecs
  -> Dot Tycon
seqDecsNE d s = product (copyUp "SeqDecsNE.T" d) [d, s]

spaceBetween
  :: Tycon
  -- ^ Boolean
  -> Dot Tycon
spaceBetween = oneConst (kind1 "SpaceBetween.T")

zenod
  :: Tycon
  -- ^ Zeroes
  -> Tycon
  -- ^ NovDecs
  -> Dot Tycon
zenod z n = product (kind1 "Zenod.T") [z, n]

zerabu
  :: Tycon
  -- ^ Zero
  -> Tycon
  -- ^ Radix
  -> Tycon
  -- ^ BU3
  -> Dot Tycon
zerabu z r b = product (copyUp "Zerabu.T" r) [z, r, b]

zeroes
  :: Tycon
  -- ^ NonZero
  -> Dot Tycon
zeroes = oneConst (kind1 "Zeroes.T")

zero :: Dot Tycon
zero = nullary (kind1 "Zero.T")

zng
  :: Tycon
  -- ^ Zero
  -> Tycon
  -- ^ NG1
  -> Dot Tycon
zng z ng1 = product (copyUp "Zng.T" ng1) [z, ng1]

znu1
  :: Tycon
  -- ^ Zero
  -> Tycon
  -- ^ Maybe Radun.T
  -> Dot Tycon
znu1 z r = product (copyUp "Znu1.T" r) [z, r]
