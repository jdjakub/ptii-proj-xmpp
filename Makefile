.PHONY: sv cl all

all: sv cl

sv: server.ml xmpp.ml xml.ml _tags
	ocamlbuild -use-ocamlfind server.native

cl: client.ml xmpp.ml xml.ml _tags
	ocamlbuild -use-ocamlfind client.byte
