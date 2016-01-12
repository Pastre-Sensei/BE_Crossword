# Custom part
SOURCES = dico_load.ml grid.ml propagation.ml backtrack.ml
#LIBS =
TARGET = bt.out

# Generic part
OCAMLC   = ocamlc -g
OCAMLDEP = ocamldep

OBJS = $(SOURCES:.ml=.cmo)

all: .depend byte
byte: $(TARGET)

$(TARGET): $(OBJS)
	$(OCAMLC) -o $@ $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmi: %.mli
	$(OCAMLC) $<

%.cmo: %.ml
	$(OCAMLC) -c $<

.PHONY: clean

clean:
	rm -f *.cm[io] *~

.depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend
