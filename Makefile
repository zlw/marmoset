prepare:
	mkdir -p bin

build: prepare
	roc build src/main.roc --output ./bin/marmoset

release: prepare
	roc build src/main.roc --optimize --output ./bin/marmoset

unit:
	roc test src/main.roc

roc-repl:
	cd src && roc repl
