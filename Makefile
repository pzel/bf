.PHONY: build test

build:
	stack build
	cp .stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/bf-exe/bf-exe ./

test:
	stack test
