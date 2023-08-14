.PHONY: build
build:
	podman build -t localhost/soitbegins-frontend .

.PHONY: watch
watch: node_modules
	rm -fr .parcel-cache && PARCEL_ELM_NO_DEBUG=1 npm start

.PHONY: format
format:
	./node_modules/.bin/elm-format src/*

.PHONY: node_modules
node_modules:
	npm ci
	touch node_modules

.PHONY: shell
shell:
	nix develop

.PHONY: nix-%
nix-%:
	nix develop \
		--command $(MAKE) $*

FORCE:
