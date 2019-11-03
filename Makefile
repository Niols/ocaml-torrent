.PHONY: build test clean

build:
	dune build @install
	ln -sf _build/install/default/bin bin
	ln -sf _build/install/default/lib lib

run: build test
	bin/torrent archlinux-2019.11.01-x86_64.iso.torrent

test: build
	dune runtest

clean:
	dune clean
	rm -f bin lib
