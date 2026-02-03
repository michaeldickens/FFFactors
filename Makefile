.SILENT:  # This stops `make` from printing every command it executes


all:
	$(MAKE) build-quiet && $(MAKE) exec


build:
	stack build


build-quiet:
	stack build --cabal-verbosity 0 --ghc-options="-fno-warn-missing-signatures -fno-warn-x-partial -fno-warn-unused-do-bind -fno-warn-incomplete-uni-patterns -fno-warn-compat-unqualified-imports"


docs:
	stack haddock --no-haddock-deps



exec:
	stack exec French-exe


open-docs:
	stack haddock --no-haddock-deps --open


run:
	$(MAKE) exec
