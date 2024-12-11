CAMLOPT = ocamlopt
CAMLFIND = ocamlfind

SRC = tools.ml crack.ml cli.ml
OBJ = $(SRC:.ml=.cmx)

.PHONY: all skibidi-cracker clean cleanup

all: skibidi-cracker

skibidi-cracker: $(OBJ)
	mkdir -p bin
	$(CAMLFIND) $(CAMLOPT) -o bin/$@ -linkpkg -package re2 -package base64 -package cryptokit $^

%.cmx: %.ml
	$(CAMLFIND) $(CAMLOPT) -c -package re2 -package base64 -package cryptokit $<

clean:
	@rm -rf bin *.cm* *.o

cleanup:
	@rm -rf *.cm* *.o
