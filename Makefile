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

# Ensure dune uses this directory as root (needed for worktrees)
DUNE_ROOT := --root $(CURDIR)

# Unit tests — run all or a specific component
# Usage: make unit              (all)
#        make unit compiler     (compiler only)
#        make unit lsp          (LSP only)
UNIT_TARGETS := compiler lsp
UNIT_COMPONENT := $(filter $(UNIT_TARGETS),$(MAKECMDGOALS))

unit:
ifeq ($(UNIT_COMPONENT),compiler)
	dune runtest $(DUNE_ROOT) lib/ --force
else ifeq ($(UNIT_COMPONENT),lsp)
	dune runtest $(DUNE_ROOT) tools/lsp/ --force
else
	dune runtest $(DUNE_ROOT) --force
endif

ifneq (,$(filter unit,$(MAKECMDGOALS)))
$(filter $(UNIT_TARGETS),$(MAKECMDGOALS)):
	@:
endif

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

treesitter:
	cd tools/tree-sitter-marmoset && npm test

watch:
	dune runtest -w --force
