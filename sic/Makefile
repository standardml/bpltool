all:	SICparser.cmo SIClex.cmo main.cmo
	ocamlmktop SICparser.cmo SIClex.cmo -o SIC.top

main.cmo:	main.ml SICparser.cmo SIClex.cmo
	ocamlc -c main.ml

SIClex.cmo:	SIClex.ml
	ocamlc -c SIClex.ml

SICparser.cmo:	SICparser.ml SICparser.cmi
	ocamlc -c SICparser.ml

SICparser.cmi:	SICparser.mli
	ocamlc -c SICparser.mli

SICparser.mli SICparser.ml:	SICparser.mly
	ocamlyacc -v SICparser.mly

SIClex.ml:	SIClex.mll SICparser.cmi
	ocamllex SIClex.mll
clean:	
	rm *.mli *.cmi *.cmo SIClex.ml SICparser.ml

