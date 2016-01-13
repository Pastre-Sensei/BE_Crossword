# Custom part
SOURCES = dico_load.ml grid.ml propagation.ml bt_v2.ml
#LIBS =
TARGET = bt.out

# Generic part
OCAMLC   = ocamlopt -g
OCAMLDEP = ocamldep

OBJS = $(SOURCES:.ml=.cmx)

all: .depend byte
byte: $(TARGET)

$(TARGET): $(OBJS)
	$(OCAMLC) -o $@ $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmi: %.mli
	$(OCAMLC) $<

%.cmx: %.ml
	$(OCAMLC) -c $<

.PHONY: clean

clean:
	rm -f *.cm[io] *~

.depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend
