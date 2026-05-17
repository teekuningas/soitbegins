.ONESHELL:

.PHONY: all shell format build earth cubemap
all: earth cubemap

shell:
	nix develop

format:
	nix develop --command black .

earth:
	nix develop --command python draw_earth.py
	cp output/earth.obj output/earth.obj.txt
	cd output && zip earth.zip earth.obj.txt

cubemap:
	nix develop --command python generate_cubemap.py

