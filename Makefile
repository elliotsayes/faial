all: build

build:
	dune build src/main.exe
	cp ./_build/default/src/main.exe ./main

clean:
	rm -f ./main
	dune clean
