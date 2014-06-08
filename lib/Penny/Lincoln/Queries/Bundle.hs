-- | Pulls fields of interest from a 'Bundle'.  This module will
-- usually get the information you are interested in. If the field you
-- seek is available only in the 'TopLine', for instance the
-- 'dateTime', the information is retrieved from there.  If it is only
-- available from the 'Posting' or from its associated 'Ent', such as
-- the 'account', it is retrieved from there.  For four fields
-- ('memo', 'number', 'flag', and 'payee') both the 'TopLine' and the
-- 'Posting' have this information.  In these cases, the function from
-- "Penny.Lincoln.Queries.Bundle.Best" is used to select the field
-- from the 'Posting' if it is not empty, or the field from the
-- 'TopLine' otherwise.
--
-- If this is not the behavior you want, use these modules instead:
--
-- * "Penny.Lincoln.Queries.Bundle.TopLine" - always select from the
-- 'TopLine'
--
-- * "Penny.Lincoln.Queries.Bundle.Posting" - always select from the
-- 'Posting'
--
-- * "Penny.Lincoln.Queries.Bundle.Siblings" - get information about
-- sibling 'Posting's
--

module Penny.Lincoln.Queries.Bundle
  ( -- * Always retrieved from TopLine
    T.topLineData
  , T.topLineMeta
  , T.dateTime
  , T.topMemoLine
  , T.topLineLine
  , T.globalTransaction
  , T.fileTransaction
  , T.filename

  -- * Pick the best field
  -- | Retrieved from Posting if field is not empty; from TopLine
  -- otherwise
  , B.number
  , B.flag
  , B.payee
  , B.memo

  -- * Always retrieved from Posting
  -- | Or from the 'Ent' associated with the 'Posting'
  , P.ent
  , P.posting
  , P.postingData
  , P.postingMeta
  , P.trio
  , P.tags
  , P.qty
  , P.commodity
  , P.entrio
  , P.account
  , P.line
  , P.globalSerial
  , P.fileSerial
  ) where

import qualified Penny.Lincoln.Queries.Bundle.Best as B
import qualified Penny.Lincoln.Queries.Bundle.TopLine as T
import qualified Penny.Lincoln.Queries.Bundle.Posting as P
