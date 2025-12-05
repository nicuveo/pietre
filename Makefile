define USAGE
Available commands:

    help       display this help
    build      builds all executables
    test       builds the project and run tests
    grammar    regenerate the grammar visualization
endef

export USAGE

help:
	@echo "$$USAGE"

build:
	stack build

test:
	stack test

grammar:
	stack run gramviz -- dot docs/pietre.gram > docs/grammar.dot
	dot -Tsvg docs/grammar.dot -o docs/grammar.svg

.PHONY: help build test grammar
