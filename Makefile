CAMLOPT = ocamlopt
CAMLFIND = ocamlfind

SRC = tools.ml crack.ml cli.ml
OBJ = $(SRC:.ml=.cmx)
PKG = re2 base64 cryptokit

.PHONY: all skibidi-cracker clean cleanup

all: skibidi-cracker

skibidi-cracker: $(OBJ)
	mkdir -p bin
	$(CAMLFIND) $(CAMLOPT) -o bin/$@ -linkpkg $(addprefix -package ,$(PKG)) $^

%.cmx: %.ml
	$(CAMLFIND) $(CAMLOPT) -c $(addprefix -package ,$(PKG)) $<

clean:
	@rm -rf bin *.cm* *.o

cleanup:
	@rm -rf *.cm* *.o
