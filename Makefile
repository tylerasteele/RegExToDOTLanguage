## Makefile for UWYO COSC 4780/5010 HW2 - Haskell side
## Author: Finley McIlwaine

.PHONY : bnfc parse compile

bnfc/*.hs: RegExp.cf
	bnfc -m -o bnfc RegExp.cf
	cd bnfc && make

bnfc: bnfc/*.hs

parse: bnfc
	@./bnfc/TestRegExp

compile: bnfc
	@cabal run -v0 hw2

compileDfa: bnfc
	@cabal run -v0 hw2 -- -x