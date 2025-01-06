build:
	dune build

release:
	dune build --profile release

unit:
	dune runtest

repl:
	dune exec marmoset

watch:
	dune build -w
