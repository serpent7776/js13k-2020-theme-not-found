.PHONY: env js dist serve

all: js dist

env:
	eval `opam env`

js:
	@mkdir -p build
	ocamlfind ocamlc -package js_of_ocaml-ppx -linkpkg -o build/game.bc src/game.ml
	js_of_ocaml -o build/game.js build/game.bc

dist: js
	@mkdir -p dist
	cp src/index.html src/style.css build/game.js dist/
	cd dist && zip -9r ../game.zip ./*
	advzip -z3 game.zip
	@als game.zip
	@printf '%s / %s\n' `stat -f '%z' game.zip` 13312

serve:
	cd dist && python -m http.server
