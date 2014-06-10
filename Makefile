SHELL := /usr/bin/env bash

default: compile

.PHONY: compile
compile: deps
	cabal --require-sandbox build -j

.PHONY: deps
deps: cabal.sandbox.config
	cabal install -j --only-dep

cabal.sandbox.config:
	cabal sandbox init
