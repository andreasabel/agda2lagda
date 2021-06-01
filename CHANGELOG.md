# Revision history for agda2lagda

## 0.2021.6.1

* Paragraphs starting with `* ` are recognized as `\item` and
  organized in an `itemize` environment.  Cannot be nested.
* Render doubly-underlined (`===`) paragraphs as `\heading`,
  dash-underlined (`---`) paragraphs as `\subheading`.
* Added a small testsuite (`cabal test`) using
  [`goldplate`](https://hackage.haskell.org/package/goldplate).
* Tested with GHC 8.10.4 and 9.0.1.

## 0.2020.11.1

* First version. Released Halloween 2020.
* Converts agda/hs files into lagda/lhs LaTeX literate files,
  turning line comments into text and block comments into
  LaTeX comments.
* Tested with GHC 8.0.2 - 8.10.3.
