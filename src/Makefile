all: setup.ml setup.data build

setup.ml: _oasis
	oasis setup

setup.data: _oasis setup.ml
	ocaml setup.ml -configure

build:
	ocaml setup.ml -build

clean:
	-ocaml setup.ml -clean

dist-clean:
	-ocaml setup.ml -clean
	-rm setup.ml setup.data _tags myocamlbuild.ml
