all:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlfind ocamlc -c type.ml
	ocamlfind ocamlc -c parser.mli
	ocamlfind ocamlc -c lexer.ml
	ocamlfind ocamlc -c parser.ml
	ocamlfind ocamlc -c bdd.ml -package extlib
	ocamlfind ocamlc -o sat -package extlib -linkpkg lexer.cmo -linkpkg parser.cmo -linkpkg bdd.cmo -linkpkg type.cmo sat.ml
