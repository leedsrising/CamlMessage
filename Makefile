default:
	ocamlbuild -use-ocamlfind mainfinal.byte && ./mainfinal.byte
	# ocamlfind ocamlopt -package lwt,lwt.unix,str -linkpkg -o mainfinal ./mainfinal.ml && ./mainfinal

clean:
	ocamlbuild -clean