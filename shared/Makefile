.ONESHELL:

.PHONY: all shell format earth cubemap
all: earth cubemap

shell:
	nix develop

format:
	black .

earth:
	python draw_earth.py
	cp output/earth.obj output/earth.obj.txt
	cd output && zip earth.zip earth.obj.txt

cubemap:
	python generate_cubemap.py

