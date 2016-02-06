-- | Number representations that are not part of the grammar.
module Penny.Rep where

import Penny.Polar
import Penny.Grammar

-- | Qty representations with a comma radix that may be either nil
-- or brim.  Stored along with the side if the number is non-nil.
type RepRadCom = Moderated NilRadCom BrimRadCom

-- | Qty representations with a period radix that may be either nil
-- or brim.  Stored along with the side if the number is non-nil.
type RepRadPer = Moderated NilRadPer BrimRadPer

-- | Qty representations that may be neutral or non-neutral and that
-- have a period or comma radix.  Stored along with the side if the
-- number is non-nil.
type RepAnyRadix = Either RepRadCom RepRadPer

-- | A non-neutral representation that does not include a side.
type BrimScalarAnyRadix = Either BrimRadCom BrimRadPer
