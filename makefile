all: expr expr_tests

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

expr_tests: expr_tests.ml
	ocamlbuild -use-ocamlfind expr_tests.byte

clean:
	rm -rf _build *.byte
