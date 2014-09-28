-- | An Ingot where the radix point is a period.
--
-- Parsing a 'T' will succeed if there is a "Penny.Tree.Currency" to
-- parse, even where there is no "Penny.Tree.Lewis" to parse.  This is
-- in contrast to "Penny.Tree.Ingot.Comma", which will fail if there
-- is a "Penny.Tree.Currency" on the left but no "Penny.Tree.Lewis";
-- the idea is that if the user enters the apostrophe, he must also
-- intend to enter a "Penny.Tree.Lewis".

module Penny.Tree.Ingot.Period where

import qualified Penny.Core.Anna as Anna
import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadPer as RadPer
import qualified Penny.Tree.Currency as Currency
import qualified Penny.Core.Commodity as Commodity
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Orient as Orient
import qualified Penny.Core.Janus as Janus
import qualified Penny.Core.Philly as Philly
import qualified Penny.Core.Walker as Walker
import qualified Penny.Core.Stokely as Stokely
import qualified Penny.Core.Polarity as Polarity
import qualified Penny.Core.Arrangement as Arrangement
import qualified Penny.Core.SpaceBetween as SpaceBetween
import Control.Applicative
import Text.Parsec.Text
import qualified Penny.Core.Trio as Trio
import qualified Penny.Tree.Ingot.Error as Error
import qualified Penny.Core.Muddy as Muddy


data T
  = Currency Currency.T (Maybe (Lewis.T RadPer.T))
  | Lewis (Lewis.T RadPer.T) (Maybe Currency.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Currency <$> Currency.parser
             <*> optional (Lewis.parser RadPer.parseRadix RadPer.parser)
  <|> Lewis <$> Lewis.parser RadPer.parseRadix RadPer.parser
            <*> optional Currency.parser

toAnna :: T -> Maybe (Anna.T RadPer.T)
toAnna (Currency _ my) = fmap Lewis.toAnna my
toAnna (Lewis lew _) = Just . Lewis.toAnna $ lew

toCurrency :: T -> Maybe Currency.T
toCurrency (Currency c _) = Just c
toCurrency (Lewis _ mayC) = mayC

toPhilly :: Lewis.T RadPer.T -> Either Error.T Philly.T
toPhilly lew = case Lewis.toAnna lew of
  Anna.Brim brim -> Right . Philly.T $ Janus.Period brim
  Anna.Nil _ -> Left $ Error.NilRepresentation

toMuddyWithSide :: Side.T -> Lewis.T RadPer.T -> Either Error.T Muddy.T
toMuddyWithSide s lew = case Lewis.toAnna lew of
  Anna.Brim brim -> Right . Muddy.T . Janus.Period
    . Walker.T . Stokely.T . Polarity.OffCenter brim $ s
  Anna.Nil _ -> Left $ Error.SideWithZeroRepresentation

toTrio
  :: Maybe (Commodity.T, Orient.T)
  -> Maybe Side.T
  -> T
  -> Either Error.T Trio.T
toTrio Nothing Nothing (Currency cy Nothing)
  = return $ Trio.C (Currency.toCommodity cy)

toTrio Nothing Nothing (Currency cy (Just lew)) = do
  phil <- toPhilly lew
  return $ Trio.UC phil (Currency.toCommodity cy)
    (Arrangement.T Orient.CommodityOnLeft (SpaceBetween.T False))

toTrio Nothing Nothing (Lewis lew Nothing) = do
  phil <- toPhilly lew
  return $ Trio.U phil

toTrio Nothing Nothing (Lewis lew (Just cy)) = do
  phil <- toPhilly lew
  return $ Trio.UC phil (Currency.toCommodity cy)
    (Arrangement.T Orient.CommodityOnRight (SpaceBetween.T False))

toTrio Nothing (Just si) (Currency cy Nothing) = return
  $ Trio.SC si (Currency.toCommodity cy)

toTrio Nothing (Just si) (Currency cy (Just lew)) = do
  muddy <- toMuddyWithSide si lew
  return $ Trio.QC muddy (Currency.toCommodity cy)
    (Arrangement.T Orient.CommodityOnLeft (SpaceBetween.T False))

toTrio Nothing (Just si) (Lewis lew Nothing) = do
  muddy <- toMuddyWithSide si lew
  return $ Trio.Q muddy

toTrio Nothing (Just si) (Lewis lew (Just curr)) = do
  muddy <- toMuddyWithSide si lew
  return $ Trio.QC muddy (Currency.toCommodity curr)
    (Arrangement.T Orient.CommodityOnRight (SpaceBetween.T False))

toTrio (Just _) _ (Currency _ _) = Left $ Error.DuplicateCommodities

toTrio (Just _) _ (Lewis _ (Just _))
  = Left $ Error.DuplicateCommodities

toTrio (Just (cy, ot)) (Just si) (Lewis lew Nothing) = do
  muddy <- toMuddyWithSide si lew
  return $ Trio.QC muddy cy
    (Arrangement.T ot (SpaceBetween.T True))

toTrio (Just (cy, ot)) Nothing (Lewis lew Nothing) = do
  phil <- toPhilly lew
  return $ Trio.UC phil cy
    (Arrangement.T ot (SpaceBetween.T True))
