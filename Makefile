SHELL := /usr/bin/env bash

default: compile

.PHONY: compile
compile: deps
	cabal build

.PHONY: deps
deps: cabal.sandbox.config
