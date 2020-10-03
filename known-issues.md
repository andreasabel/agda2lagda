
##  Known issues

Strings containing the closing comment delimiter "-}" confuse the
lexer and may produce unexpected output.  At the time of writing, this
is already a problem in Agda, see
https://github.com/agda/agda/issues/4953.
Similar problems exist in the languages Haskell and Idris (and C, Java, HTML, ...).
