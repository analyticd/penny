module Penny.Tree.Ingot.Comma where

import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna as Anna
import qualified Penny.Core.Anna.RadCom as RadCom
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
import qualified Penny.Tree.Apostrophe as Apostrophe
import Control.Applicative
import Text.Parsec.Text
import qualified Penny.Core.Trio as Trio
import qualified Penny.Tree.Ingot.Error as Error
import qualified Penny.Core.Muddy as Muddy

data T
  = T Apostrophe.T
      (Either (Currency.T, Lewis.T RadCom.T)
              (Lewis.T RadCom.T, Maybe Currency.T))
      Apostrophe.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = T
  <$> Apostrophe.parser
  <*> ( fmap Left ((,) <$> Currency.parser
                       <*> Lewis.parser RadCom.parseRadix RadCom.parser)
        <|> fmap Right ( (,) <$> Lewis.parser
                                 RadCom.parseRadix RadCom.parser
                             <*> optional Currency.parser))
  <*> Apostrophe.parser

toTrio
  :: Maybe (Commodity.T, Orient.T)
  -> Maybe Side.T
  -> T
  -> Either Error.T Trio.T
toTrio mp ms (T _ ei _) = eiToTrio mp ms ei

toAnna :: T -> Anna.T RadCom.T
toAnna (T _ ei _) = case ei of
  Left (_, lew) -> Lewis.toAnna lew
  Right (lew, _) -> Lewis.toAnna lew


toCurrency :: T -> Maybe Currency.T
toCurrency (T _ ei _) = case ei of
  Left (c, _) -> Just c
  Right (_, mayC) -> mayC


eiToTrio
  :: Maybe (Commodity.T, Orient.T)
  -> Maybe Side.T
  -> Either (Currency.T, Lewis.T RadCom.T)
            (Lewis.T RadCom.T, Maybe Currency.T)
  -> Either Error.T Trio.T
eiToTrio Nothing Nothing (Left (curr, lew)) =
  case Lewis.toAnna lew of
    Anna.Brim brim -> Right $ Trio.UC phil (Currency.toCommodity curr)
      (Arrangement.T Orient.CommodityOnLeft (SpaceBetween.T False))
      where
        phil = Philly.T $ Janus.Comma brim
    Anna.Nil _ -> Left $ Error.CurrencyWithNilRepresentation

eiToTrio Nothing Nothing (Right (lew, Just curr)) = do
  phil <- toPhilly lew
  return $ Trio.UC phil (Currency.toCommodity curr)
    (Arrangement.T Orient.CommodityOnRight (SpaceBetween.T False))

eiToTrio Nothing Nothing (Right (lew, Nothing)) = do
  phil <- toPhilly lew
  return $ Trio.U phil

eiToTrio (Just _) Nothing (Left _) =
  Left $ Error.DuplicateCommodities

eiToTrio (Just _) Nothing (Right (_, Just _)) =
  Left $ Error.DuplicateCommodities

eiToTrio (Just (cy, ot)) Nothing (Right (lew, Nothing)) = do
  phil <- toPhilly lew
  return $ Trio.UC phil cy (Arrangement.T ot (SpaceBetween.T False))

eiToTrio Nothing (Just si) (Left (curr, lew)) = do
  mud <- toMuddyWithSide si lew
  return $ Trio.QC mud (Currency.toCommodity curr)
    (Arrangement.T Orient.CommodityOnLeft (SpaceBetween.T False))

eiToTrio Nothing (Just si) (Right (lew, Nothing)) = do
  muddy <- toMuddyWithSide si lew
  return $ Trio.Q muddy

eiToTrio (Just _) _ (Left _) = Left Error.DuplicateCommodities

eiToTrio (Just _) _ (Right (_, Just _)) = Left Error.DuplicateCommodities

eiToTrio (Just (cy, ot)) (Just si) (Right (lew, Nothing)) = do
  muddy <- toMuddyWithSide si lew
  return $ Trio.QC muddy cy
    (Arrangement.T ot (SpaceBetween.T True))

eiToTrio Nothing (Just si) (Right (lew, Just curr)) = do
  muddy <- toMuddyWithSide si lew
  return $ Trio.QC muddy (Currency.toCommodity curr)
    (Arrangement.T Orient.CommodityOnRight (SpaceBetween.T False))

toPhilly :: Lewis.T RadCom.T -> Either Error.T Philly.T
toPhilly lew = case Lewis.toAnna lew of
  Anna.Brim brim -> Right . Philly.T $ Janus.Comma brim
  Anna.Nil _ -> Left $ Error.NilRepresentation

toMuddyWithSide :: Side.T -> Lewis.T RadCom.T -> Either Error.T Muddy.T
toMuddyWithSide s lew = case Lewis.toAnna lew of
  Anna.Brim brim -> Right . Muddy.T . Janus.Comma
    . Walker.T . Stokely.T . Polarity.OffCenter brim $ s
  Anna.Nil _ -> Left $ Error.SideWithZeroRepresentation

