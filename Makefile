format = native
root = cli.$(format)
file = data/test_basic.rb

build:
	ocamlbuild -use-menhir -use-ocamlfind -quiet bin/$(root)

run: build
	./$(root) $(file)

clean:
	corebuild -clean

.PHONY: build clean run
