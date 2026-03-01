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

integration-legacy:
	@./test/integration.sh $(filter-out integration-legacy,$(MAKECMDGOALS))

ifneq (,$(filter integration-legacy,$(MAKECMDGOALS)))
$(filter-out integration-legacy,$(MAKECMDGOALS)):
	@:
endif

integration-fixtures:
	@./test/integration.sh 99_fixtures.sh

repl:
	@echo "REPL is removed. Use 'make run file=examples/fibonacci-typed.mr'." && exit 1

run: release
	@./marmoset $(file)

treesitter:
	cd tools/tree-sitter-marmoset && npm test

watch:
	dune runtest -w --force

# CI entrypoints (stable command contract for GitHub workflows)
ci-compiler-unit:
	@$(MAKE) unit compiler

ci-lsp-unit:
	@$(MAKE) unit lsp

ci-compiler-integration-linux:
	@$(MAKE) integration

ci-lsp-integration:
	@./test/integration.sh 08_cli.sh

ci-quality-lint-fmt-doc:
	@./test/ci/quality.sh

ci-editor-zed:
	@./test/ci/editor-zed.sh

ci-editor-vscode:
	@./test/ci/editor-vscode.sh

ci-editor-nvim:
	@./test/ci/editor-nvim.sh

ci-editor-jetbrains:
	@./test/ci/editor-jetbrains.sh
