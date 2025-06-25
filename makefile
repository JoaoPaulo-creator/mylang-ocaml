build:
	dune build
clean:
	dune clean

run:
	./_build/install/default/bin/mylang-ocaml file.my
