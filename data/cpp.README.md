[![Hackage version](https://img.shields.io/hackage/v/agda2lagda.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/agda2lagda)
[![agda2lagda on Stackage Nightly](https://stackage.org/package/agda2lagda/badge/nightly)](https://stackage.org/nightly/package/agda2lagda)
[![Stackage LTS version](https://www.stackage.org/package/agda2lagda/badge/lts?label=Stackage)](https://www.stackage.org/package/agda2lagda)
[![Cabal build](https://github.com/andreasabel/agda2lagda/workflows/Haskell-CI/badge.svg)](https://github.com/andreasabel/agda2lagda/actions)
[![Stack build](https://github.com/andreasabel/agda2lagda/workflows/Stack%20build/badge.svg)](https://github.com/andreasabel/agda2lagda/actions)
<!-- [![Travis build status](https://travis-ci.com/andreasabel/agda2lagda.svg?branch=master)](https://travis-ci.com/andreasabel/agda2lagda) -->


agda2lagda: Convert Agda/Haskell text to literate Agda/Haskell text
===================================================================

Generate a LaTeX literate Agda/Haskell script from an Agda/Haskell script.

- Single line comments are turned into ordinary LaTeX.
  * Paragraphs followed by a line of equal signs are turned into `\heading`s.
  * Paragraphs followed by a line of dashes are turned into `\subheading`s.
  * Consecutive paragraphs starting `*` are turned into an `itemize` environment.
  * At the end of the file, extra block comment terminators are removed.

- Comment blocks, if started on the 0th column, count as _commenting out_.
  These will be turned into TeX comments.
  Nested comments are not recognized.

- The rest is interpreted as code and wrapped in a `code` environment.

Example: `agda2lagda Foo.agda`

Input: `Foo.agda`
```agda
@include test/Foo.agda
```

Output: `Foo.lagda.tex`
```latex
@include test/golden/Foo.lagda.tex
```

## Installation

These are standard installation instructions.

Last update of installation instructions: 2021-05-29.

### From stackage.org

Requires [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
```
stack update
stack install agda2lagda
```

### From hackage.haskell.org

Requires GHC >= 8.0 and the Haskell Cabal.
```
cabal update
cabal install agda2lagda
```

### From the repository

```
git clone https://github.com/andreasabel/agda2lagda.git
cd agda2lagda
cabal install
```
Alternatively to the last line, you can use `stack`.
E.g. if you have GHC 8.10.2 installed, you can use this compiler as follows:
```
stack install --system-ghc --stack-yaml stack-8.10.2.yaml
```
Alternatively, `stack` can install the compiler for you:
```
stack install --stack-yaml stack-xx.yy.zz.yaml
```
The `xx.yy.zz` is a placeholder for the GHC version,
choose one (for which there is a `.yaml` file).

At the time of writing, installation with these GHC versions has been tested:
8.0.2, 8.2.2, 8.4.4, 8.6.5, 8.8.4, 8.10.4, 9.0.1.
