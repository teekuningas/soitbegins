.ONESHELL:

.PHONY: all
all: earth

.PHONY: earth
earth:
	python draw_earth.py
	cp output/earth.obj output/earth.obj.txt
	cd output
	zip earth.zip earth.obj.txt
