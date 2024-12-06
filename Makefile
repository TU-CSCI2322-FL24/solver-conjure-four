# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc --make -O -o connectfour Main.hs

prof:
	ghc --make -prof -o connectfour Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f connectfour
	rm -f *.hi
	rm -f *.o

setup:
	cabal install ansi-terminal
