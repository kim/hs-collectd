SHELL := /usr/bin/env bash

CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox


default: compile

.PHONY: compile
compile: deps
	cabal --require-sandbox build -j

.PHONY: deps
deps: cabal.sandbox.config
	cabal --require-sandbox install -j --only-dep

.PHONY: dist
dist: compile
	cabal sdist

cabal.sandbox.config:
	cabal sandbox init --sandbox=$(CABAL_SANDBOX)
