{-# OPTIONS_GHC -Wall #-}
-- | This file is a sample of how to configure penny-fit. You will
-- have to adapt it for your own needs. Rename the file to
-- FILENAME.hs, where FILENAME is the name you want for your program
-- (an easy choice is "penny-fit".) Then, compile the file with
--
-- ghc --make FILENAME
--
-- and if all goes well you will have a program to use.
module Main where

import Penny.Brenner
import Data.Version

-- | The Config type configures all the financial institution accounts.
config :: Config
config = Config
  { defaultFitAcct = Nothing
    -- ^ If you have a default financial institution account, use Just
    -- ACCOUNT here. For example, if I wanted to use my amex account
    -- by default, I would put @Just amex@ here. I don't want to use
    -- an account by default; I want to be required to explicitly
    -- state an account, so I put Nothing here.

  , moreFitAccts = [ visa, checking, saving ]
    -- ^ This is a list of financial institution accounts in addition
    -- to the default (if you have one.)
  }

visa :: FitAcct
visa = FitAcct
  { fitAcctName = "visa"
    -- ^ This is the name by which you will identify this account on
    -- the command line (it will be case sensitive). You pick
    -- financial institution accounts by using the @-f@ option.

  , fitAcctDesc = unlines
      [ "Main Visa card account."
      , "To find the downloads, log in and then click on"
      , "Statements --> Download --> Quicken."
      ]
    -- ^ A description of the financial institution account. This
    -- appears when you use the @info@ command, so you can put
    -- information in here like where to find the downloads on the
    -- bank website.

  , dbLocation = "/home/massysett/ledger/visa"
    -- ^ The location of the database of financial institution
    -- postings. You can make this path relative, in which case it is
    -- interpreted relative to the current directory at runtime, or
    -- absolute. No expansion of tildes, environment variables,
    -- etc. is performed.

  , pennyAcct = "Liabilities:Current:Amex"
    -- ^ Postings from this financial institution appear under this
    -- account in your ledger file(s).

  , defaultAcct = "Expenses:Unclassified"
    -- ^ When penny-fit finds a financial institution posting and it
    -- does not have a matching posting in your ledger, it must create
    -- a new transaction with two postings. One posting will be in the
    -- pennyAcct specified above and the other posting will be in this
    -- account.

  , currency = "$"
    -- ^ All postings will be in this currency

  , groupSpecs = GroupSpecs NoGrouping NoGrouping
    -- ^ When penny-fit reprints your ledger, it uses this to
    -- determine how to perform digit grouping for the quantities in
    -- the output. This takes the form
    --
    -- GroupSpecs G1 G2
    --
    -- where G1 and G2 can each be
    --
    -- NoGrouping, GroupLarge, or GroupAll
    --
    -- G1 specifies how to group digits to the left of the decimal
    -- point. G2 specifies how to group digits to the right of the
    -- decimal point. All grouping creates groups of three digits.
    --
    -- NoGrouping means do not do any digit grouping at
    -- all.
    --
    -- GroupLarge means perform digit grouping, but only if the
    -- number to be grouped is greater than 9,999 (if grouping to the
    -- left of the decimal point) or if there are more than 4 decimal
    -- places (if grouping to the right of the decimal point.)
    --
    -- GroupAll means group whenever there are at least four digits to
    -- be grouped.

  , translator = IncreaseIsCredit
    -- ^ Postings from your financial institution are specified in
    -- terms of increases or decreases. Postings in your ledger are
    -- specified in terms of debits or credits. The translator
    -- specifies how to convert a posting from your financial
    -- institution to a posting in the pennyAcct in your ledger. For
    -- deposit accounts (e.g. checking) you will typically use
    -- IncreaseIsDebit; for liability accounts (e.g. credit cards) you
    -- will typically use IncreaseIsCredit.

  , side = CommodityOnLeft
    -- ^ When penny-fit creates new postings it must put the commodity
    -- either to the left of the quantity or to the right of the
    -- quantity. Accordingly your choices here are CommodityOnLeft or
    -- CommodityOnRight.

  , spaceBetween = NoSpaceBetween
    -- ^ When penny-fit creates new postings it must decide whether to
    -- put a space between the commodity and the quantity. Accordingly
    -- your choices here are SpaceBetween or NoSpaceBetween.

  , parser = ofxParser
    -- ^ This determines how to parse the data you have
    -- downloaded. Currently there is only one parser, which handles
    -- OFX data. Many financial institutions provide OFX data so this
    -- will get you a long way. If you want to write additional
    -- parsers you can provide your own function of this type (perhaps
    -- your bank only provides CSV files, for example.)

  , toLincolnPayee = usePayeeOrDesc
  -- ^ Sometimes the financial institution provides Payee information,
  -- sometimes it does not. Sometimes the Desc might have additional
  -- information that you might want to remove. This function can be
  -- used to do that. The resulting Lincoln Payee is used for any
  -- transactions that are created by the merge command. The resulting
  -- payee is also used when comparing new financial institution
  -- postings to already existing ledger transactions in order to
  -- guess at which payee and accounts to create in the transactions
  -- created by the merge command.
  --
  -- 'usePayeeOrDesc' simply uses the payee if it is available;
  -- otherwise, it uses the description. (Many banks provide
  -- descriptions only and do not provide separate payee information.)
  }

checking :: FitAcct
checking = FitAcct
  { fitAcctName = "checking"
  , fitAcctDesc = "Main checking account."
  , dbLocation = "/home/massysett/ledger/checking"
  , pennyAcct = "Assets:Current:Checking"
  , defaultAcct = "Expenses:Unclassified"
  , currency = "$"
  , groupSpecs = GroupSpecs NoGrouping NoGrouping
  , translator = IncreaseIsDebit
  , side = CommodityOnLeft
  , spaceBetween = NoSpaceBetween
  , toLincolnPayee = usePayeeOrDesc
  , parser = ofxParser
  }

saving :: FitAcct
saving = FitAcct
  { fitAcctName = "saving"
  , fitAcctDesc = "Main saving account."
  , dbLocation = "/home/massysett/ledger/saving"
  , pennyAcct = "Assets:Current:Checking:Omari"
  , defaultAcct = "Expenses:Unclassified"
  , currency = "$"
  , groupSpecs = GroupSpecs NoGrouping NoGrouping
  , translator = IncreaseIsDebit
  , side = CommodityOnLeft
  , spaceBetween = NoSpaceBetween
  , toLincolnPayee = usePayeeOrDesc
  , parser = ofxParser
  }

-- Leave things below this line alone (unless you know what you're
-- doing of course.)

-- brennerMain requires that you supply a version. You can edit this
-- as you see fit, or use the version that Cabal supplies.
version :: Version
version = Version [1] []

-- | Always leave these two lines the same (unless you know what you
-- are doing of course).
main :: IO ()
main = brennerMain version config
