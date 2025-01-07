build:
	dune build

release:
	dune build --profile release

unit:
	dune runtest --force

repl:
	dune exec marmoset

watch:
	dune runtest -w --force
