[![Hackage version](https://img.shields.io/hackage/v/agda2lagda.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/agda2lagda)
[![agda2lagda on Stackage Nightly](https://stackage.org/package/agda2lagda/badge/nightly)](https://stackage.org/nightly/package/agda2lagda)
[![Stackage LTS version](https://www.stackage.org/package/agda2lagda/badge/lts?label=Stackage)](https://www.stackage.org/package/agda2lagda)
[![Cabal build](https://github.com/andreasabel/agda2lagda/workflows/Haskell-CI/badge.svg)](https://github.com/andreasabel/agda2lagda/actions)
[![Stack build](https://github.com/andreasabel/agda2lagda/workflows/Stack%20build/badge.svg)](https://github.com/andreasabel/agda2lagda/actions)



agda2lagda: Convert program to literate program (Agda/Haskell)
==============================================================

Generate a literate Agda/Haskell script from an Agda/Haskell script.
Produces LaTeX or Markdown literate scripts.

Specification
-------------

Conversion into LaTeX (default):

- Single line comments are turned into ordinary LaTeX.
  * Paragraphs followed by a line of equal signs are turned into `\heading`s.
  * Paragraphs followed by a line of dashes are turned into `\subheading`s.
  * Consecutive paragraphs starting `*` are turned into an `itemize` environment.
  * At the end of the file, extra block comment terminators are removed.

- Comment blocks, if started on the 0th column, count as _commenting out_.
  These will be turned into TeX comments.
  Nested comments are not recognized.

- The rest is interpreted as code and wrapped in a `code` environment.

Conversion into Markdown (option `--markdown`) is similar,
but nothing needs to be done for headings and itemize environments:

- Single line comments are turned into ordinary text.
  * At the end of the file, extra block comment terminators are removed.

- Comment blocks, if started on the 0th column, count as _commenting out_.
  These will be turned into HTML comments.
  Nested comments are not recognized.

- The rest is interpreted as code and wrapped in a code environment (triple backticks).

Examples
--------

Given input [`Foo.agda`](https://github.com/andreasabel/agda2lagda/blob/master/test/Foo.agda):
- Invocation `agda2lagda Foo.agda` produces output
  [`Foo.lagda.tex`](https://github.com/andreasabel/agda2lagda/blob/master/test/golden/Foo.lagda.tex).
- Invocation `agda2lagda --markdown Foo.agda` produces output
  [`Foo.lagda.md`](https://github.com/andreasabel/agda2lagda/blob/master/test/golden/Foo.lagda.md).

LaTeX examples (rendered):
- http://www.cse.chalmers.se/~abela/#MultiSortedAlgebra
- http://www.cse.chalmers.se/~abela/#cr-sk

Example `Makefile` to turn `.agda` file into highlighted HTML via Markdown (since `v0.2023.1.12`):
```make
FILE=Foo
TITLE=The Title

default : md/html/$(FILE).html

# Step 1: agda2lagda: Produce Markdown literate script.

md/%.lagda.md : %.agda
	agda2lagda -f --markdown -o md/ $<

# Step 2: agda: Highlight and format code blocks as HTML.
# Also produces Agda.css.

md/html/%.md : md/%.lagda.md
	cd md ; agda --html --html-highlight=auto ../$<

# Step 3: pandoc: Produce HTML.  Improvise header to make HTML self-contained.

md/html/%.html : md/html/%.md
	echo '<!DOCTYPE HTML><html><head><meta charset="utf-8"><title>$(TITLE)</title><link rel="stylesheet" href="Agda.css"></head>' > $@
	pandoc -f markdown -t html $< >> $@
```

Installation from binary
------------------------

Binaries for Linux, macOS and Windows are available from [GitHub releases](https://github.com/andreasabel/agda2lagda/releases).
Just download the executable for your platform there and put it in a directory that is in the system `PATH`.

For example, under Linux (similar under macOS):
```shell
VERSION="0.2023.1.12"
SRC="https://github.com/andreasabel/agda2lagda/releases/download/v${VERSION}/agda2lagda-${VERSION}-linux.binary"
TGT="/usr/local/bin/agda2lagda"
wget ${SRC} -O ${TGT}
chmod +x ${TGT}
```
For macOS, there is also a installer package, e.g.:
```shell
VERSION="0.2023.1.12"
SRC="https://github.com/andreasabel/agda2lagda/releases/download/v${VERSION}/agda2lagda-${VERSION}-mac.pkg"
TGT="/tmp/agda2lagda.pkg"
wget ${SRC} -O ${TGT}
open ${TGT}
```
Verify the installation with these commands (Linux/macOS):
```console
$ which agda2lagda
/usr/local/bin/agda2lagda
$ agda2lagda --version
agda2lagda version 0.2023.1.12
```

Installation from source
------------------------

These are standard installation instructions.

Last update of installation instructions: 2023-01-11.

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
E.g. if you have GHC 8.10.7 installed, you can use this compiler as follows:
```
stack install --system-ghc --stack-yaml stack-8.10.7.yaml
```
Alternatively, `stack` can install the compiler for you:
```
stack install --stack-yaml stack-xx.yy.zz.yaml
```
The `xx.yy.zz` is a placeholder for the GHC version,
choose one (for which there is a `.yaml` file).

At the time of writing, installation with these GHC versions has been tested:
8.0.2, 8.2.2, 8.4.4, 8.6.5, 8.8.4, 8.10.7, 9.0.2, 9.2.5, 9.4.4.
