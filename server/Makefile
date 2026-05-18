.PHONY: shell
shell:
	nix develop

.PHONY: watch
watch:
	python3 server.py

.PHONY: format
format:
	black server.py scripts/test_client.py

.PHONY: build
build:
	nix build .#dockerImage
	podman load -i result

.PHONY: nix-%
nix-%:
	nix develop \
		--command $(MAKE) $*

FORCE:

