expand-includes=data/expand-includes.awk

.PHONY: all
all : test README.md

# OBSOLETE, hackage is fixed:
# # Atm, hackage renders comments verbatim, so we need to delete them.
# # https://github.com/haskell/hackage-server/issues/937
# README.md : data/cpp.README.md $(expand-includes) Makefile
# 	@echo "Regenerating $@."
# 	@rm $@
# #	@echo "<!-- DO NOT EDIT me directly, as I am generated from $< ! -->" > $@
# #	@echo "" >> $@
# 	@$(expand-includes) $< | strip-html-comments.sed >> $@

.PHONY: test
test:
	cabal test

.PHONY: accept fix-tests
accept: fix-tests
fix-tests:
	cabal exec -- goldplate --fix test

.PHONY: test-help
test-help:
	cabal run agda2lagda -- --help

.PHONY: version
version:
	cabal run agda2lagda -- --version

.PHONY: quick-test
quick-test:
	cabal run agda2lagda -- -v --force -o test/out/ test/Foo.agda
	cabal run agda2lagda -- -v --force -o test/Foo-generated.lagda test/Foo.agda

.PHONY: bug
bug:
	cabal run agda2lagda -- --dry-run test/ClosingCommentInString.agda

.PHONY: install
install:
	cabal install

.PHONY: haddock
haddock:
	cabal v1-haddock --executables

# To install bash completion,
# agda2lagda needs to be installed on the PATH.
# (Only needs to be installed once, even if options of agda2lagda change.)

.PHONY: install-bash-completion
install-bash-completion:
	install-bash-completion.sh agda2lagda

# EOF
