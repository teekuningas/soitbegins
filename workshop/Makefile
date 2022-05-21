.ONESHELL:

.PHONY: all
all: earth

.PHONY: earth
earth:
	python draw_earth.py
	cp output/earth.obj output/earth.obj.txt

.PHONY: serve
serve:
	cd output
	python ../serve.py

