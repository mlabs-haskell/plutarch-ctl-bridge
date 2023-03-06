# This really ought to be `/usr/bin/env bash`, but nix flakes don't like that.
SHELL := /bin/sh

.PHONY: hoogle format haddock usage tag format_nix format_haskell format_check	\
	lint refactor ps_bridge bench bench_check scripts test build ci

HASKELL_SOURCES := $(shell fd -ehs)
CABAL_SOURCES := $(shell fd -ecabal)
NIX_SOURCES := $(shell fd -enix)
FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications	\
			-o -XImportQualifiedPost -o -XPatternSynonyms -o -XOverloadedRecordDot
HLINT_EXTS := -XQuasiQuotes

THREADS ?= 8
PS_BRIDGE_OUTPUT_DIR ?= agora-purescript-bridge/
BENCH_OUTPUT ?= bench.csv
TEST_CASE_TIMEOUT ?= 100

usage:
	@echo "usage: [env [<variable>=<value> ...]] make <command> [OPTIONS]"
	@echo
	@echo "Available variables:"
	@echo "  THREADS -- The number of threads for building the project"
	@echo "  TEST_CASE_TIMEOUT -- Timeout for individual tests. Default unit: s"
	@echo 
	@echo "Available commands:"
	@echo "  hoogle -- Start local hoogle"
	@echo "  format -- Format the project"
	@echo "  haddock -- Generate Haddock docs for project"
	@echo "  tags -- Generate CTAGS and ETAGS files for project"
	@echo "  format_check -- Check if all files have been formatted correctly"
	@echo "  lint -- Get hlint suggestions for project"
	@echo "  build -- Compile project"
	@echo "  test -- Run all tests for project"
	@echo "  ci -- Run all the CI checks"

requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)

hoogle: requires_nix_shell
	pkill hoogle || true
	hoogle generate --local=haddock --database=hoo/local.hoo
	hoogle server --local -p 8081 >> /dev/null &
	hoogle server --local --database=hoo/local.hoo -p 8082 >> /dev/null &

format: 
	nixpkgs-fmt $(NIX_SOURCES)
	fourmolu $(FORMAT_EXTENSIONS) -m inplace $(HASKELL_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

format_check: requires_nix_shell
	fourmolu $(FORMAT_EXTENSIONS) -m check $(HASKELL_SOURCES)
	nixpkgs-fmt --check $(NIX_SOURCES) 
	cabal-fmt --check $(CABAL_SOURCES)

haddock: requires_nix_shell
	cabal haddock --haddock-html --haddock-hoogle --builddir=haddock

tag: requires_nix_shell
	hasktags -x $(HASKELL_SOURCES)

tags:
	make tag

lint: requires_nix_shell
	hlint $(HLINT_EXTS) $(HASKELL_SOURCES)

refactor: requires_nix_shell
	for src in $(HASKELL_SOURCES) ; do \
		hlint $(HLINT_EXTS) --refactor --refactor-options='-i -s' $$src ;\
	done

test: requires_nix_shell
	cabal test --test-options="--hide-successes -t $(TEST_CASE_TIMEOUT) -j$(THREADS)"

build: requires_nix_shell
	cabal build -j$(THREADS)

ci:
	@ [[ "$$(uname -sm)" == "Linux x86_64" ]] || (echo "NOTE: CI only builds on Linux x86_64. Your system is $$(uname -sm), continuing...")
	nix build .#check.$(shell nix eval -f '<nixpkgs>' system)
