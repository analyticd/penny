#!/bin/sh

# regenerate.sh - regenerates HTML and PDF man pages.  $1 is the
# source directory.  The output goes to the current directory.

find $1 -type f \( -name '*.7' -o -name '*.1' \) -print | \
    while read line; do
        base=`basename $line`
        # HTML output
        groff -mandoc -Thtml $line > $base.html

        # PDF output
        groff -mandoc -t -Tps $line > $base.ps
        ps2pdf $base.ps $base.pdf
        rm $base.ps
    done
