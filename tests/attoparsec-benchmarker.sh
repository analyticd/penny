#!/bin/sh

# This script benchmarks the attoparsec parser vs. the parsec
# parser. The data is all generated from the
# PennyTest.Penny.Copper.Gen.Parsers module, and the code in the
# attoparsec and parsec parsers is nearly identical, so this should
# produce an accurate comparison between the two parsers. So far the
# results are not showing that the attoparsec parser is much
# faster. It is measurably faster, but I do not think the speed
# improvement is significant enough to merit maintaining an attoparsec
# parser and a parsec parser (maintaining attoparsec alone is not an
# option, because of its lack of location tracking features.)

# Also of interest is a previous setup that used a parser with Happy
# and Alex. See the "old-alex" tag, which is commit
# c530b946a53ffb201f3933152acf5339d22f09d7. That parser also did not
# prove to have a significant speed difference from the parsec parser.

# The only other speedup option remaining is to write a parser with
# Happy and Alex, but to significantly change the grammar to shift
# more of the work to the lexer. The parser in the "old-alex" branch
# used a fairly simple lexer that created a heapload of tokens, which
# left a lot of work for the Happy parser. However I doubt that
# changing the grammar would produce a significantly faster parser,
# and such a change would be time consuming and difficult to document.

# Before running the benchmark make sure that the proper versions of
# penny-lib and penny-tests are installed. The output of "time" will
# go to standard error so if you want to send the output to a file be
# sure to redirect standard out and standard error.

# Some sample results have been committed in the test-results
# file. They show attoparsec running in 18.4 seconds and parsec in
# 21.2 seconds. This is consistent with other results, which show
# attoparsec being measurably faster but not by enough of a margin to
# make it worthwhile to maintain two parsers.

# With this, all efforts to write alternative parsers will
# cease. Penny parsers will be in parsec for the foreseeable
# future. Any future speedup efforts will focus on other areas
# (caching perhaps?) but it is not worth it to investigate any other
# parsers.

time penny-test --plain -t Parsec/ledger
time penny-test --plain -t Atto/ledger
