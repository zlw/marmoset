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
	@./_build/default/bin/main.exe

run: release
	@./_build/default/bin/main.exe $(file)

watch:
	dune runtest -w --force
