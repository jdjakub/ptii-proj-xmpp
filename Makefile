.PHONY: sv cl all tests

all: sv cl tests

sv: server.ml xmpp.ml xml.ml _tags
	ocamlbuild -use-ocamlfind server.native

cl: client.ml xmpp.ml xml.ml _tags
	ocamlbuild -use-ocamlfind client.byte

tests: tests.ml client.ml xmpp.ml xml.ml _tags
	ocamlbuild -use-ocamlfind tests.native
