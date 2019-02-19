FORMAT ?= exe
FILE ?= data/test_kitchen_sink.rb

build:
	esy refmterr dune build bin/cli.$(FORMAT)

run:
	esy refmterr dune exec bin/cli.$(FORMAT) $(FILE)

test:
	esy refmterr dune runtest

clean:
	esy dune clean

.PHONY: build clean run test
