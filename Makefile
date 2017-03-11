all: _oasis
	ocaml setup.ml -build

setup:
	oasis setup && ocaml setup.ml -configure

clean:
	ocaml setup.ml -clean
	find -name *.mllib -exec rm -f {} \;
	find -name *.mldylib -exec rm -f {} \;
	find -name *.byte -exec rm -f {} \;
	rm -rf bin/*
	rm -f setup.ml

test:
	ocaml setup.ml -test

simple:
	./scripts/test_simple.sh run

hard:
	./scripts/test_hard.sh run

example:
	./scripts/example.sh $(program)

.PHONY: all setup clean test simple hard example
