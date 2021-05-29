.PHONY : bug haddock install quick-test test test-help version

expand-includes=data/expand-includes.awk

all : test README.md

README.md : data/cpp.README.md $(expand-includes)
	@echo "Regenerating $@."
	@echo "<!-- DO NOT EDIT me directly, as I am generated from $< ! -->" > $@
	@echo "" >> $@
	@$(expand-includes) $< >> $@

test :
	cabal test

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
