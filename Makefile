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

repl:
	@echo "REPL is removed. Use 'make run file=examples/fibonacci-typed.mr'." && exit 1

run: release
	@./_build/install/default/bin/marmoset $(file)

watch:
	dune runtest -w --force
