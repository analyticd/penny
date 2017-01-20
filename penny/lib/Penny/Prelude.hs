-- | An alternative Prelude for Penny.
--
-- Currently this is not used everywhere in Penny.  It was created
-- after switching over to the @system-filepath@ package, which is
-- what @turtle@ uses.  This switch cleaned up a lot of miscellaneous
-- types and synonyms that were being used for paths and for file IO.
--
-- Thus now it is used primarily in modules that contain the
-- 'FilePath' type; since those modules would have had conflicts with
-- the Haskell Prelude anyway, it was worth making a clean break from
-- the Haskell Prelude in those modules.
--
-- This module imports only from non-Penny packages.
module Penny.Prelude
  ( -- * Comparisons
    Eq ((==))
  , Ord (compare, (<=), (<), (>=), (>), max, min)

  -- * Showing
  , Show (show)

  -- * Accuerr
  , Accuerr (AccSuccess, AccFailure)
  , _AccSuccess
  , _AccFailure
  , accuerrToEither

  -- * File paths
  , FilePath
  , filePathToText

  -- * Generics
  , Generic

  -- * Text
  , Text
  , pack
  , unpack
  , Data.Text.Encoding.encodeUtf8

  -- * Errors and exceptions
  , Exception
  , throwIO
  , error

  -- * Typeable
  , Typeable

  -- * Non-empty sequences
  , NonEmptySeq
  , nonEmptySeqToSeq

  -- * Locations
  , Loc(Loc)

  -- * Numeric types
  , Int
  , Prelude.negate

  -- * Characters
  , Char

  -- * Functions
  , (.)
  , (&)
  , ($)
  , id
  , Prelude.const
  , Prelude.uncurry
  , (Category.>>>)
  , (Category.<<<)

  -- * Typeclasses
  , Traversable(traverse)
  , Functor(fmap)
  , Enum(succ, pred)

  -- * Foldable
  , Foldable (toList)
  , Foldable.any
  , Foldable.all
  , Foldable.foldr
  , Foldable.foldl'

  -- * Monad
  , Monad((>>=), (>>), return, fail)
  , join
  , Monad.guard

  -- * Applicative
  , Applicative.Applicative (pure, (<*>), (*>), (<*))
  , Applicative.Alternative (empty, (<|>), some, many)

  -- * Sequences
  , Seq
  , zip
  , zipWith

  -- * Booleans
  , Bool(True, False)
  , (Prelude.&&)
  , (Prelude.||)
  , Prelude.not
  , Prelude.otherwise

  -- * Sum types
  , Either(Left, Right)
  , either

  -- * Maybe
  , Maybe(Nothing, Just)
  , maybe
  , Maybe.fromMaybe

  -- * Anonymous sums
  , S3(S3_1, S3_2, S3_3)
  , Sums.Prisms._S3_1, Sums.Prisms._S3_2, Sums.Prisms._S3_3

  -- * IO
  , IO
  , readFile
  , writeFile
  , appendFile
  , MonadIO
  , liftIO

  -- * ByteString
  , ByteString

  -- * Monoids
  , (<>)
  , Prelude.Monoid (mempty, mappend)

  -- * Tuples
  , fst
  , snd

  -- * Date and time
  , Time.Day
  , Time.ZonedTime

  -- * Rainbow
  , Rainbow.Chunk

  -- * Lens
  , Lens.view
  , Lens.Wrapped (_Wrapped')
  , Lens.over
  , Lens.set
  , Lens._1
  , Lens._2
  , Lens._Left
  , Lens._Right

  -- * Maps
  , Map.Map

  -- * Sets
  , Set.Set
  ) where

import Accuerr (Accuerr(AccSuccess, AccFailure), _AccSuccess, _AccFailure,
  accuerrToEither)
import qualified Control.Applicative as Applicative
import Data.ByteString (ByteString)
import qualified Control.Category as Category
import Control.Exception (Exception, throwIO)
import qualified Control.Lens as Lens
import Control.Monad (join)
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import Data.Sequence (Seq, zip, zipWith)
import Data.Sequence.NonEmpty (NonEmptySeq, nonEmptySeqToSeq)
import qualified Data.Time as Time
import Data.Foldable (Foldable, toList)
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import qualified Data.Text.Encoding
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem.Path.CurrentOS (toText)
import Sums (S3(S3_1, S3_2, S3_3))
import qualified Sums.Prisms
import Pinchot(Loc(Loc))
import qualified Rainbow

import qualified Prelude
import Prelude
  ( Eq((==)), Ord(compare), Show(show),
    Either(Left, Right), Maybe(Nothing, Just),
    Int, Char, Traversable(traverse), IO,
    Monad((>>=), (>>)), (.),
    Enum(succ, pred), Bool(True, False),
    Functor(fmap), error, ($), either, maybe, id, fail,
    return, fst, snd, (&&), (||)
  )

filePathToText :: FilePath -> Either Text Text
filePathToText = toText

readFile :: FilePath -> IO Text
readFile = XIO.readFile
  . either (fail . ("could not decode filename: " <>) . X.unpack)
           X.unpack
  . toText

writeFile :: FilePath -> Text -> IO ()
writeFile fp txt = do
  fp' <- case filePathToText fp of
    Left e -> fail ("could not decode filename: " <> X.unpack e)
    Right g -> return . X.unpack $ g
  XIO.writeFile fp' txt

appendFile :: FilePath -> Text -> IO ()
appendFile fp txt = do
  fp' <- case filePathToText fp of
    Left e -> fail ("could not decode filename: " <> X.unpack e)
    Right g -> return . X.unpack $ g
  XIO.appendFile fp' txt

