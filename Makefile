.PHONY: build dev sass tailwind 

build:
	elm make src/Main.elm --output=dist/index.js

dev:
	elm-live src/Main.elm --start-page=dist/index.html --open -- --output=dist/index.js

sass:
	sass --watch src/styles:dist/styles

tailwind:
	npx tailwindcss build src/styles/globals.css -o dist/styles/tailwind.css
