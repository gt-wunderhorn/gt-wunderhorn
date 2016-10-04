OCB_FLAGS = -tag thread -use-ocamlfind -pkgs $(LIBS) -Is $(DIRS)
OCB = ocamlbuild $(OCB_FLAGS)
DIRS = utility

LIBS = sawja

NAME = main

all: byte

use:
	$(OCB) use.byte
	mkdir parsed

clean:
	$(OCB) -clean
	rm -rf parsed

byte:
	$(OCB) $(NAME).byte

test: all
	./test.sh

.PHONY: all clean byte native debug test
