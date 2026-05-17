.PHONY: shell watch format build

shell:
	nix develop

watch:
	python3 simulation_server.py

format:
	nix develop --command black .

build:
	nix build .#dockerImage
	podman load -i result

.PHONY: nix-%
nix-%:
	nix develop \
		--command $(MAKE) $*

FORCE: