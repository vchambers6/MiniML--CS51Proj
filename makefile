all: evaluation miniml expr tests

evaluation: evaluation.ml 
	ocamlbuild evaluation.byte

miniml: miniml.ml
	ocamlbuild miniml.byte

expr: expr.ml
	ocamlbuild expr.byte

tests: tests.ml
	ocamlbuild tests.byte

clean:
	rm -rf _build *.byte