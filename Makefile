.SILENT:  # This stops `make` from printing every command it executes

all:
	stack build && stack exec FFFactors-exe

build:
	stack build

docs:
	stack haddock --no-haddock-deps

exec:
	stack exec FFFactors-exe

open-docs:
	stack haddock --no-haddock-deps --open

run:
	$(MAKE) exec
