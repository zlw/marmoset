install:
	opam install --deps-only --with-test .

clean:
	@rm -rf _build
	@rm -f ./marmoset
	@rm -f ./marmoset-lsp

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

lsp:
	dune build tools/lsp/main.exe
	@cp -f _build/default/tools/lsp/main.exe ./marmoset-lsp
	@chmod +x ./marmoset-lsp

watch:
	dune runtest -w --force
