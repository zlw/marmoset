install:
	opam install --deps-only --with-test .

clean:
	@rm -rf _build
	@rm -f ./marmoset

build:
	dune build

release:
	dune build --profile release ./bin/main.exe
	@cp -f _build/default/bin/main.exe ./marmoset
	@chmod +x ./marmoset

unit:
	dune runtest --force

integration:
	@./test/integration.sh $(filter-out integration,$(MAKECMDGOALS))

# Allow: make integration <suite> (e.g. make integration unions)
ifneq (,$(filter integration,$(MAKECMDGOALS)))
$(filter-out integration,$(MAKECMDGOALS)):
	@:
endif

repl:
	@echo "REPL is removed. Use 'make run file=examples/fibonacci-typed.mr'." && exit 1

run: release
	@./marmoset $(file)

watch:
	dune runtest -w --force
