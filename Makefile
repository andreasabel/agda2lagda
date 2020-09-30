test :
	cabal run agda2lagda -- --help
	cabal run agda2lagda -- -v --force test/Foo.agda

install :
	cabal install

haddock :
	cabal v1-haddock --executables

# EOF
