OCB_FLAGS = -tag thread -use-ocamlfind -pkgs $(LIBS) -Is $(DIRS)
OCB = ocamlbuild $(OCB_FLAGS)
DIRS = scripts,src,src/utility

LIBS = sawja

NAME = main

all: byte

use:
	$(OCB) use.byte
	mv use.byte bin

clean:
	$(OCB) -clean
	rm -rf parsed

byte:
	$(OCB) $(NAME).byte
	mv $(NAME).byte bin

test: all
	scripts/test.sh

.PHONY: all clean byte native debug test
