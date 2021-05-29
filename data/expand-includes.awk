#!/usr/local/bin/gawk -f

# https://unix.stackexchange.com/questions/527252/tool-script-that-can-expand-all-include-directives-in-a-makefile

# 2021-05-29
# Expand lines of the form
#
#   @include file-name-without-spaces
#
# by inlining such files.

{
     if (NF == 2 && $1 == "@include") {
          while ((getline line < $2) > 0)
               print line
          close($2)
     } else
          print
}

# EOF
