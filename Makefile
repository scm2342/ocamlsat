POPT=a

all: parserpp
	ocamlfind ocamlc -c type.ml
	ocamlfind ocamlc -c parser.mli
	ocamlfind ocamlc -c lexer.ml
	ocamlfind ocamlc -c parser.ml
	ocamlfind ocamlc -c bdd.ml
	ocamlfind ocamlc -o sat -package extlib -linkpkg lexer.cmo -linkpkg parser.cmo -linkpkg bdd.cmo -linkpkg type.cmo sat.ml

parserpp:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	
prof: parserpp
	make -C extlib-1.5.2 POPT=$(POPT)
	ocamlcp -p $(POPT) -c type.ml
	ocamlcp -p $(POPT) -c parser.mli
	ocamlcp -p $(POPT) -c lexer.ml
	ocamlcp -p $(POPT) -c parser.ml
	ocamlcp -p $(POPT) -c bdd.ml
	ocamlcp -p $(POPT) -I extlib-1.5.2 -c sat.ml
	ocamlcp -p $(POPT) -I extlib-1.5.2 -o sat extlib-1.5.2/extLib.cma type.cmo lexer.cmo parser.cmo bdd.cmo sat.cmo

opt: parserpp
	ocamlfind ocamlopt -c type.ml
	ocamlfind ocamlopt -c parser.mli
	ocamlfind ocamlopt -c lexer.ml
	ocamlfind ocamlopt -c parser.ml
	ocamlfind ocamlopt -c bdd.ml
	ocamlfind ocamlopt -o satopt -package extlib -linkpkg lexer.cmx -linkpkg parser.cmx -linkpkg bdd.cmx -linkpkg type.cmx sat.ml

clean:
	rm -f type.{cmi,cmo,cmx,o} parser.{ml,mli,cmi,cmo,cmx,o} lexer.{ml,cmi,cmo,cmx,o} bdd.{cmi,cmo,cmx,o} sat{,.cmi,.cmo,opt,.cmx,.o} pigeon{,.hi,.o} ocamlprof.dump

pigeon:
	ghc -O2 --make pigeon.hs

test: all pigeon
	time ./pigeon 9 | ./sat
