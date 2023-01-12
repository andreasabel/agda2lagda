.PHONY : bug haddock install quick-test test test-help version

expand-includes=data/expand-includes.awk

all : test README.md

# Atm, hackage renders comments verbatim, so we need to delete them.
# https://github.com/haskell/hackage-server/issues/937
README.md : data/cpp.README.md $(expand-includes) Makefile
	@echo "Regenerating $@."
	@rm $@
#	@echo "<!-- DO NOT EDIT me directly, as I am generated from $< ! -->" > $@
#	@echo "" >> $@
	@$(expand-includes) $< | strip-html-comments.sed >> $@

test :
	cabal test

fix-tests:
	cabal exec -- goldplate --fix test

test-help:
	cabal run agda2lagda -- --help

version :
	cabal run agda2lagda -- --version

quick-test :
	cabal run agda2lagda -- -v --force -o test/out/ test/Foo.agda
	cabal run agda2lagda -- -v --force -o test/Foo-generated.lagda test/Foo.agda

bug :
	cabal run agda2lagda -- --dry-run test/ClosingCommentInString.agda

install :
	cabal install

haddock :
	cabal v1-haddock --executables

# To install bash completion,
# agda2lagda needs to be installed on the PATH.
# (Only needs to be installed once, even if options of agda2lagda change.)

install-bash-completion :
	install-bash-completion.sh agda2lagda

# EOF
