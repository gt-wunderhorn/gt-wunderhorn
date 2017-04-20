all: _oasis
	ocaml setup.ml -build

setup:
	oasis setup && ocaml setup.ml -configure

clean:
	ocaml setup.ml -clean || true
	find -name '*.mllib' -delete
	find -name '*.mldylib' -delete
	find -name '*.byte' -delete
	rm -rf bin/*
	rm -f setup.ml
	git clean -xdf

test:
	ocaml setup.ml -test

simple:
	./scripts/test_simple.sh run

hard:
	./scripts/test_hard.sh run

example:
	./scripts/example.sh $(program)

.PHONY: all setup clean test simple hard example
