.PHONY: all clean install build
all: build doc

NAME=forkexec
J=4

BINDIR ?= /usr/bin
SBINDIR ?= /usr/sbin
ETCDIR ?= /etc
DESTDIR ?= /

export OCAMLRUNPARAM=b

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure

build: setup.data setup.bin version.ml
	@./setup.bin -build -j $(J)

version.ml: VERSION
	echo "let version = \"$(shell cat VERSION)\"" > lib/version.ml

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install
	mkdir -p $(DESTDIR)/$(ETCDIR)/init.d
	install ./src/init.d-fe $(DESTDIR)/$(ETCDIR)/init.d/fe
	install ./fe_main.native $(DESTDIR)/$(SBINDIR)/xcp-fe
	install ./fe_cli.native $(DESTDIR)/$(BINDIR)/xcp-fe-cli

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall
	mkdir -p $(DESTDIR)/$(ETCDIR)/init.d
	install ./src/init.d-fe $(DESTDIR)/$(ETCDIR)/init.d/fe
	install ./fe_main.native $(DESTDIR)/$(SBINDIR)/xcp-fe
	install ./fe_cli.native $(DESTDIR)/$(BINDIR)/xcp-fe-cli

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(DESTDIR)/$(ETCDIR)/init.d/fe
	rm -f $(DESTDIR)/$(SBINDIR)/xcp-fe
	rm -f $(DESTDIR)/$(BINDIR)/xcp-fe-cli

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
