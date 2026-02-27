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

integration:
	@./test/integration.sh $(filter-out integration,$(MAKECMDGOALS))

# Allow: make integration <suite> (e.g. make integration trait)
ifneq (,$(filter integration,$(MAKECMDGOALS)))
$(filter-out integration,$(MAKECMDGOALS)):
	@:
endif

repl:
	@echo "REPL is removed. Use 'make run file=examples/fibonacci-typed.mr'." && exit 1

run: release
	@./_build/install/default/bin/marmoset $(file)

watch:
	dune runtest -w --force
