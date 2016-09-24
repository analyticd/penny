{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | Optics for the @lens@ library that correspond to the types in
-- "Penny.Copper.Types" are in this module.
--
-- These optics are written by hand.
module Penny.Copper.Optics.Manual where

import Penny.Copper.Types
import Penny.Copper.Grammar
import Pinchot (rulesToOptics)

import qualified Control.Lens as Lens

-- * 'WholeFile' optics

-- | Gets the first file item in the 'WholeFile'.
firstFileItem :: Lens.Traversal' (WholeFile t a) (FileItem t a)
firstFileItem
  = r'WholeFile'0'FileItemP'Star
  . Lens._Wrapped'
  . Lens._Cons
  . Lens._1
  . r'FileItemP'1'FileItem

-- | Gets every item in the 'WholeFile' except for the first.
tailFileItems :: Lens.Traversal' (WholeFile t a) (FileItem t a)
tailFileItems
  = r'WholeFile'0'FileItemP'Star
  . Lens._Wrapped'
  . Lens._Cons
  . Lens._2
  . traverse
  . r'FileItemP'1'FileItem

-- | Maps over every 'FileItem' in the 'WholeFile'.
allFileItems :: Lens.Traversal' (WholeFile t a) (FileItem t a)
allFileItems
  = r'WholeFile'0'FileItemP'Star
  . Lens._Wrapped'
  . traverse
  . r'FileItemP'1'FileItem

-- | Every 'FileItemP'.
allFileItemP :: Lens.Traversal' (WholeFile t a) (FileItemP t a)
allFileItemP
  = r'WholeFile'0'FileItemP'Star
  . Lens._Wrapped'
  . traverse

-- | Ever 'FileItemP' except the first one in the file.
tailFileItemP :: Lens.Traversal' (WholeFile t a) (FileItemP t a)
tailFileItemP
  = r'WholeFile'0'FileItemP'Star
  . Lens._Wrapped'
  . Lens._Cons
  . Lens._2
  . traverse

-- | Maps over every 'Transaction' in the 'WholeFile'.
allTransactions :: Lens.Traversal' (WholeFile t a) (Transaction t a)
allTransactions = allFileItems . _FileItem'Transaction

-- | Maps over every Posting.
allPostings :: Lens.Traversal' (WholeFile t a) (Posting t a)
allPostings
  = allTransactions
  . r'Transaction'2'Postings
  . r'Postings'1'PostingList'Opt
  . Lens._Wrapped'
  . Lens._Just
  . eachPosting
  where
    eachPosting f (PostingList ws p1 ps)
      = PostingList
      <$> pure ws
      <*> f p1
      <*> (Lens._Wrapped' . traverse . r'NextPosting'3'Posting) f ps

-- | Every origDate.
allOrigDate :: Lens.Traversal' (WholeFile t a) (DateTimeZone t a)
allOrigDate
  = allPostings
  . postingFieldsInPosting
  . eachPostingFieldInPostingFields
  . _PostingField'OrigDate
  . r'OrigDate'2'DateTimeZone

-- | Maps over every 'Price' in the 'WholeFile'.
allPrices :: Lens.Traversal' (WholeFile t a) (Price t a)
allPrices = allFileItems . _FileItem'Price

-- | Maps over every 'Comment' in the 'WholeFile'.
allComments :: Lens.Traversal' (WholeFile t a) (Comment t a)
allComments = allFileItems . _FileItem'Comment

-- | Every 'DateTimeZone' contained in the TopLine.
topLineDates :: Lens.Traversal' (WholeFile t a) (DateTimeZone t a)
topLineDates
  = allTransactions
  . r'Transaction'0'TopLineFields
  . r'TopLineFields'0'DateField
  . Lens._Wrapped'

-- * 'Posting' optics

-- | The PostingFields in a Posting which has a trio.
postingFieldsInPostingWithTrio
  :: Lens.Traversal' (Posting t a) (PostingFields t a)
postingFieldsInPostingWithTrio
  = _Posting'TrioMaybeFields
  . r'TrioMaybeFields'1'PostingFieldsP'Opt
  . Lens._Wrapped'
  . Lens._Just
  . r'PostingFieldsP'1'PostingFields

-- | The PostingFields in a Posting that does not have a Trio.
postingFieldsInPostingNoTrio
  :: Lens.Traversal' (Posting t a) (PostingFields t a)
postingFieldsInPostingNoTrio
  = _Posting'PostingFields

-- | The PostingFields in a Posting, regardless of whether it has a
-- Trio.
postingFieldsInPosting :: Lens.Traversal' (Posting t a) (PostingFields t a)
postingFieldsInPosting f (Posting'TrioMaybeFields tmf)
  = Posting'TrioMaybeFields <$> get f tmf
  where
    get = r'TrioMaybeFields'1'PostingFieldsP'Opt
        . Lens._Wrapped'
        . Lens._Just
        . r'PostingFieldsP'1'PostingFields
postingFieldsInPosting f (Posting'PostingFields pf)
  = Posting'PostingFields <$> f pf

-- * 'PostingFields' optics

-- | Every 'PostingField' in a 'PostingFields'.
eachPostingFieldInPostingFields
  :: Lens.Traversal' (PostingFields t a) (PostingField t a)
eachPostingFieldInPostingFields f (PostingFields p1 ps)
  = PostingFields
  <$> f p1
  <*> (Lens._Wrapped' . traverse . r'PostingFieldP'1'PostingField) f ps


-- * 'DateTimeZone' optics

zoneInDateTimeZone :: Lens.Traversal' (DateTimeZone t a) (WhitesZone'Opt t a)
zoneInDateTimeZone
  = r'DateTimeZone'1'TimeAndMayZone'Opt
  . Lens._Wrapped'
  . Lens._Just
  . r'TimeAndMayZone'1'WhitesZone'Opt



-- * Transaction optics

whitesBetweenTopLineAndPostings
  :: Lens.Lens' (Transaction t a) (White'Star t a)
whitesBetweenTopLineAndPostings = r'Transaction'1'White'Star

whitesBetweenOpenCurlyAndFirstPosting
  :: Lens.Traversal' (Transaction t a) (White'Star t a)
whitesBetweenOpenCurlyAndFirstPosting
  = r'Transaction'2'Postings
  . r'Postings'1'PostingList'Opt
  . Lens._Wrapped'
  . Lens._Just
  . r'PostingList'0'White'Star

whitesBetweenPostingEndAndSemi
  :: Lens.Traversal' (Transaction t a) (White'Star t a)
whitesBetweenPostingEndAndSemi
  = r'Transaction'2'Postings
  . r'Postings'1'PostingList'Opt
  . Lens._Wrapped'
  . Lens._Just
  . r'PostingList'2'NextPosting'Star
  . Lens._Wrapped'
  . traverse
  . r'NextPosting'0'White'Star

whitesBetweenSemiAndPosting :: Lens.Traversal' (Transaction t a) (White'Star t a)
whitesBetweenSemiAndPosting
  = r'Transaction'2'Postings
  . r'Postings'1'PostingList'Opt
  . Lens._Wrapped'
  . Lens._Just
  . r'PostingList'2'NextPosting'Star
  . Lens._Wrapped'
  . traverse
  . r'NextPosting'2'White'Star

-- | After the optional posting list.  The posting list might be empty.
whitesAfterPostingList :: Lens.Traversal' (Transaction t a) (White'Star t a)
whitesAfterPostingList
  = r'Transaction'2'Postings
  . r'Postings'2'White'Star

-- * Whitespace optics

-- | Separators between labels and the field in a 'PostingField'.
sepPostingField :: Lens.Traversal' (PostingField t a) (White'Star t a)
sepPostingField f x = case x of
  PostingField'Number n -> PostingField'Number <$> r'Number'1'White'Star f n
  PostingField'Flag a -> pure (PostingField'Flag a)
  PostingField'Account a -> pure (PostingField'Account a)
  PostingField'Fitid a -> PostingField'Fitid <$> r'Fitid'1'White'Star f a
  PostingField'Tags a -> pure (PostingField'Tags a)
  PostingField'Uid a -> PostingField'Uid <$> r'Uid'1'White'Star f a
  PostingField'OfxTrn a -> PostingField'OfxTrn <$> r'OfxTrn'1'White'Star f a
  PostingField'OrigDate a -> PostingField'OrigDate <$> r'OrigDate'1'White'Star f a

-- | Separators within the 'Trio'.  Does not traverse the 'SemanticSpace's.
sepTrio :: Lens.Traversal' (Trio t a) (White'Star t a)
sepTrio f x = case x of
  Trio'T_DebitCredit a -> Trio'T_DebitCredit <$> pure a
  Trio'T_DebitCredit_Commodity a -> Trio'T_DebitCredit_Commodity <$>
    r'T_DebitCredit_Commodity'1'White'Star f a
  Trio'T_DebitCredit_Commodity_NonNeutral (T_DebitCredit_Commodity_NonNeutral
    dc w1 cy w2 nn) -> Trio'T_DebitCredit_Commodity_NonNeutral <$>
      (T_DebitCredit_Commodity_NonNeutral <$> pure dc <*> f w1
        <*> pure cy <*> pure w2 <*> pure nn)
  Trio'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral
    dc w1 nn) -> Trio'T_DebitCredit_NonNeutral <$>
      (T_DebitCredit_NonNeutral <$> pure dc <*> f w1
        <*> pure nn)
  Trio'T_DebitCredit_NonNeutral_Commodity (T_DebitCredit_NonNeutral_Commodity
    dc w1 nn w2 cy) -> Trio'T_DebitCredit_NonNeutral_Commodity <$>
    (T_DebitCredit_NonNeutral_Commodity <$> pure dc <*> f w1 <*> pure nn
      <*> pure w2 <*> pure cy)
  Trio'T_Commodity a -> Trio'T_Commodity <$> pure a
  Trio'T_Commodity_Neutral (T_Commodity_Neutral cy w1 neu) ->
    Trio'T_Commodity_Neutral <$> (T_Commodity_Neutral <$> pure cy
      <*> pure w1 <*> pure neu)
  Trio'T_Neutral_Commodity (T_Neutral_Commodity neu w1 cy) ->
    Trio'T_Neutral_Commodity <$> (T_Neutral_Commodity <$> pure neu
      <*> pure w1 <*> pure cy)
  Trio'T_Commodity_NonNeutral (T_Commodity_NonNeutral cy w1 neu) ->
    Trio'T_Commodity_NonNeutral <$> (T_Commodity_NonNeutral <$> pure cy
      <*> pure w1 <*> pure neu)
  Trio'T_NonNeutral_Commodity (T_NonNeutral_Commodity neu w1 cy) ->
    Trio'T_NonNeutral_Commodity <$> (T_NonNeutral_Commodity <$> pure neu
      <*> pure w1 <*> pure cy)
  Trio'T_Neutral n -> Trio'T_Neutral <$> pure n
  Trio'T_NonNeutral n -> Trio'T_NonNeutral <$> pure n

sepPostingFieldP :: Lens.Traversal' (PostingFieldP t a) (White'Star t a)
sepPostingFieldP = r'PostingFieldP'1'PostingField . sepPostingField

sepPostingFields :: Lens.Traversal' (PostingFields t a) (White'Star t a)
sepPostingFields f (PostingFields pf pfStar)
  = PostingFields
  <$> sepPostingField f pf
  <*> (Lens._Wrapped' . traverse . sepPostingFieldP) f pfStar

sepPostingFieldsP :: Lens.Traversal' (PostingFieldsP t a) (White'Star t a)
sepPostingFieldsP f (PostingFieldsP ws pf)
  = PostingFieldsP
  <$> f ws
  <*> sepPostingFields f pf

sepOptPostingFieldsP :: Lens.Traversal' (PostingFieldsP'Opt t a) (White'Star t a)
sepOptPostingFieldsP = Lens._Wrapped' . Lens._Just . sepPostingFieldsP

sepTrioMaybeFields :: Lens.Traversal' (TrioMaybeFields t a) (White'Star t a)
sepTrioMaybeFields f (TrioMaybeFields t pf)
  = TrioMaybeFields
  <$> sepTrio f t
  <*> sepOptPostingFieldsP f pf

sepPosting :: Lens.Traversal' (Posting t a) (White'Star t a)
sepPosting f (Posting'TrioMaybeFields tmf) = Posting'TrioMaybeFields
  <$> sepTrioMaybeFields f tmf
sepPosting f (Posting'PostingFields pf) = Posting'PostingFields
  <$> sepPostingFields f pf

sepNextPosting :: Lens.Traversal' (NextPosting t a) (White'Star t a)
sepNextPosting f (NextPosting w0 si w1 p)
  = NextPosting
  <$> pure w0
  <*> pure si
  <*> pure w1
  <*> sepPosting f p

sepNextPosting'Star :: Lens.Traversal' (NextPosting'Star t a) (White'Star t a)
sepNextPosting'Star = Lens._Wrapped' . traverse . sepNextPosting

sepPostingList :: Lens.Traversal' (PostingList t a) (White'Star t a)
sepPostingList f (PostingList w0 p1 ps)
  = PostingList
  <$> pure w0
  <*> sepPosting f p1
  <*> sepNextPosting'Star f ps

sepPostingList'Opt :: Lens.Traversal' (PostingList'Opt t a) (White'Star t a)
sepPostingList'Opt = Lens._Wrapped' . Lens._Just . sepPostingList

sepPostings :: Lens.Traversal' (Postings t a) (White'Star t a)
sepPostings = r'Postings'1'PostingList'Opt . sepPostingList'Opt

sepOrigPayee :: Lens.Traversal' (OrigPayee t a) (White'Star t a)
sepOrigPayee f (OrigPayee ws0 lbl ws1 str) =
  OrigPayee <$> f ws0 <*> pure lbl <*> f ws1 <*> pure str

sepOrigPayee'Opt :: Lens.Traversal' (OrigPayee'Opt t a) (White'Star t a)
sepOrigPayee'Opt = Lens._Wrapped' . Lens._Just . sepOrigPayee

sepTopLineFields :: Lens.Traversal' (TopLineFields t a) (White'Star t a)
sepTopLineFields = r'TopLineFields'3'OrigPayee'Opt . sepOrigPayee'Opt

sepTransaction :: Lens.Traversal' (Transaction t a) (White'Star t a)
sepTransaction f (Transaction tlf w pstgs)
  = Transaction
  <$> sepTopLineFields f tlf
  <*> pure w
  <*> sepPostings f pstgs

sepPrice :: Lens.Traversal' (Price t a) (White'Star t a)
sepPrice = r'Price'1'White'Star

sepFileItem :: Lens.Traversal' (FileItem t a) (White'Star t a)
sepFileItem f x = case x of
  FileItem'Price p -> FileItem'Price <$> sepPrice f p
  FileItem'Transaction t -> FileItem'Transaction <$> sepTransaction f t
  FileItem'Comment a -> FileItem'Comment <$> pure a

sepFileItemP :: Lens.Traversal' (FileItemP t a) (White'Star t a)
sepFileItemP = r'FileItemP'1'FileItem . sepFileItem

sepFileItemP'Star :: Lens.Traversal' (FileItemP'Star t a) (White'Star t a)
sepFileItemP'Star = Lens._Wrapped' . traverse . sepFileItemP

sepWholeFile :: Lens.Traversal' (WholeFile t a) (White'Star t a)
sepWholeFile f (WholeFile fs ws)
  = WholeFile
  <$> sepFileItemP'Star f fs
  <*> pure ws
