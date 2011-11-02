all:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlfind ocamlc -c type.ml
	ocamlfind ocamlc -c parser.mli
	ocamlfind ocamlc -c lexer.ml
	ocamlfind ocamlc -c parser.ml
	ocamlfind ocamlc -c bdd.ml
	ocamlfind ocamlc -o sat -package extlib -linkpkg lexer.cmo -linkpkg parser.cmo -linkpkg bdd.cmo -linkpkg type.cmo sat.ml

opt:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlfind ocamlopt -c type.ml
	ocamlfind ocamlopt -c parser.mli
	ocamlfind ocamlopt -c lexer.ml
	ocamlfind ocamlopt -c parser.ml
	ocamlfind ocamlopt -c bdd.ml
	ocamlfind ocamlopt -o satopt -package extlib -linkpkg lexer.cmx -linkpkg parser.cmx -linkpkg bdd.cmx -linkpkg type.cmx sat.ml

clean:
	rm -f type.{cmi,cmo,cmx,o} parser.{ml,mli,cmi,cmo,cmx,o} lexer.{ml,cmi,cmo,cmx,o} bdd.{cmi,cmo,cmx,o} sat{,.cmi,.cmo,opt,.cmx,.o} pigeon{,.hi,.o}

pigeon:
	ghc -O2 --make pigeon.hs

test: all pigeon
	time ./pigeon 9 | ./sat
