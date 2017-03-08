all: _oasis
	ocaml setup.ml -build

setup:
	oasis setup && ocaml setup.ml -configure

clean:
	ocaml setup.ml -clean
	find -name *.mllib   | xargs rm
	find -name *.mldylib | xargs rm
	find -name *.byte    | xargs rm
	rm bin/*
	rm setup.ml

test:
	ocaml setup.ml -test

simple:
	./scripts/test_simple.sh run

hard:
	./scripts/test_hard.sh run

example:
	./scripts/example.sh $(program)

.PHONY: all setup clean test simple hard example
