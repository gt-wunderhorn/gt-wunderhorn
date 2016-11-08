OCB_FLAGS = -tag thread -use-ocamlfind -pkgs $(LIBS) -Is $(DIRS)
OCB = ocamlbuild $(OCB_FLAGS)
DIRS = scripts,src,src/utility,src/shape,unit

LIBS = sawja,Z3,core

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

unit:
	$(OCB) unit.byte
	mv unit.byte bin
	bin/unit.byte

.PHONY: all clean byte native debug test unit
