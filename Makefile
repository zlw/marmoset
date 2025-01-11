install:
	opam install --deps-only --with-test .

clean:
	@rm -rf _build

build:
	dune build

release:
	dune build --profile release

unit:
	dune runtest --force

repl: release
	@./_build/install/default/bin/marmoset

run: release
	@./_build/install/default/bin/marmoset $(file)

watch:
	dune runtest -w --force
