.POSIX:

PREFIX = /usr/local

all: build

build:
	cabal build

dist:
	mkdir -p daffm-build
	make PREFIX=daffm-build install
	cp -R LICENSE daffm-build
	tar -cf - daffm-build | gzip > daffm.tar.gz
	rm -rf daffm-build

install:
	cabal install -g -O2 --install-method=copy --overwrite-policy=always --installdir="$(PREFIX)/bin/"
	install -Dm644 "./docs/daffm.1" "$(PREFIX)/share/man/man1/daffm.1"

uninstall:
	rm -f "$(PREFIX)/bin/daffm"
	rm -f "$(PREFIX)/share/man/man1/daffm.1"

# Generate markdown doc from manpage
doc:
	pandoc -f man -t markdown docs/daffm.1 -o docs/daffm.md

.PHONY: build install doc
