CAMLOPT = ocamlopt
CAMLFIND = ocamlfind

SRC = tools.ml crack.ml cli.ml

.PHONY: all skibidi clean

all: skibidi-cracker

skibidi-cracker:
	mkdir -p bin
	$(CAMLFIND) $(CAMLOPT) -c -package re2 -package base64 -package cryptokit tools.ml
	$(CAMLFIND) $(CAMLOPT) -o bin/$@ -linkpkg -package re2 -package base64 -package cryptokit tools.cmx crack.ml cli.ml

clean:
	@rm -rf bin *.cm* *.o

cleanup:
	@rm -rf *.cm* *.o
