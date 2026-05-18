.PHONY: build
build:
	podman build -t localhost/soitbegins-frontend .

.PHONY: watch
watch: node_modules assets
	rm -fr .parcel-cache && PARCEL_ELM_NO_DEBUG=1 npm start

.PHONY: assets
assets:
	cp ../shared/output/elevation_*.png static/

.PHONY: format
format:
	nix develop --command elm-format --yes src/*

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
