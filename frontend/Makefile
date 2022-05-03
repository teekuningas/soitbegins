build:
	rm -fr dist/*
	node_modules/.bin/parcel build index.html

watch:
	rm -fr .parcel-cache && PARCEL_ELM_NO_DEBUG=1 npm start

prettier:
	./node_modules/.bin/elm-format src/*

install:
	npm install
