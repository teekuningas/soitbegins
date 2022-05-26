.ONESHELL:

.PHONY: all
all: earth

.PHONY: earth
earth:
	python draw_earth.py
	cp output/earth.obj output/earth.obj.txt
	sed -i '/^vn/d' output/earth.obj.txt
	sed -i '/^#/d' output/earth.obj.txt
	cd output
	zip earth.zip earth.obj.txt

.PHONY: serve
serve:
	cd output
	python ../serve.py

