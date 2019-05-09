all: evaluation miniml expr

evaluation: evalutaion.ml 
	ocamlbuild evaluation.byte

miniml: miniml.ml
	ocamlbuild miniml.byte

expr: expr.ml
	ocamlbuild expr.byte

clean:
	rm -rf _build *.byte