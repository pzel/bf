.PHONY: build example test

build:
	stack build
	cp .stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/bf-exe/bf-exe ./bf

example: test build
	./bf ./bf-src/hello.bf

test:
	stack test
