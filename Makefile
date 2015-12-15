# Custom part
SOURCES = dico_load.ml grid2.ml propagation.ml
#LIBS =
TARGET = executable

# Generic part
OCAMLC   = ocamlc -g
OCAMLDEP = ocamldep

OBJS = $(SOURCES:.ml=.cmo)

all: .depend byte
byte: $(TARGET)

$(TARGET): $(OBJS)
	$(OCAMLC) -o $@ $(@) $^

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
