.PHONY: shell watch format build test sweep visualize

shell:
	nix develop

watch:
	python3 simulation_server.py

test:
	python3 test_mesh.py && python3 test_3d_physics.py

sweep:
	python3 sweep.py --frames 300

visualize:
	python3 visualize.py --snapshot 500 --all-layers --field T

format:
	black .

build:
	nix build .#dockerImage
	podman load -i result

.PHONY: nix-%
nix-%:
	nix develop \
		--command $(MAKE) $*

FORCE: