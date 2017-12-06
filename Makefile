default:
	ocamlbuild -use-ocamlfind mainfinal.byte && ./mainfinal.byte
	#ocamlfind ocamlopt -package lwt,lwt.unix,str -linkpkg -o mainfinal ./networking2.ml ./suggest.ml ./messageTransformer.ml ./command.ml ./state.ml ./mainfinal.ml && ./mainfinal

test:
		ocamlbuild -use-ocamlfind messageTransformer_test.byte && ./messageTransformer_test.byte

clean:
	ocamlbuild -clean
