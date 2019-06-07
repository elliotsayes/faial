OCB_FLAGS   = -use-ocamlfind -use-menhir -I src -no-links
OCB = ocamlbuild $(OCB_FLAGS)

BUILD = _build/src

all: native byte

clean:
	$(OCB) -clean

native:
	$(OCB) main.native
	$(OCB) test_loops.native
	cp $(BUILD)/main.native main

test:
	@$(BUILD)/test_loops.native

byte:
	$(OCB) main.byte

profile:
	$(OCB) -tag profile main.native

debug:
	$(OCB) -tag debug main.byte

.PHONY: all clean byte native profile debug sanity test
