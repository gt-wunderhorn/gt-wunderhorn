OCB_FLAGS = -tag thread -use-ocamlfind -pkgs $(LIBS) -Is $(DIRS)
OCB = ocamlbuild $(OCB_FLAGS)
DIRS = utility

LIBS = batteries,sawja

NAME = example

all: byte

use:
	$(OCB) use.byte

clean:
	$(OCB) -clean

native:
	$(OCB) $(NAME).native

byte:
	$(OCB) $(NAME).byte

test: native
	./$(NAME).native "OCaml" "OCamlBuild" "users"

.PHONY: all clean byte native debug test
