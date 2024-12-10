CAMLOPT = ocamlopt
CAMLFIND = ocamlfind

SRC = tools.ml cli.ml

.PHONY: all skibidi clean

all: skibidi-cracker

skibidi-cracker:
	mkdir -p bin
	$(CAMLFIND) $(CAMLOPT) -o bin/$@ -linkpkg -package base64 -package cryptokit -package re2 $(SRC)

clean:
	@rm -rf bin *.cm* *.o

cleanup:
	@rm -rf *.cm* *.o
