.PHONY : bug haddock install test test-help version

test-help:
	cabal run agda2lagda -- --help

version :
	cabal run agda2lagda -- --version

test :
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
