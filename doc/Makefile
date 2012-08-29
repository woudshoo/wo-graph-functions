all: pdf html info

IMAGE_SOURCE = graph-1-in.dot graph-1-out.dot graph-1b-out.dot
MAIN_SOURCE = wo-graph-functions.texinfo

HTML_IMAGES = ${IMAGE_SOURCE:.dot=.dot.png} 

include-stamp: ../*.lisp ../*.asd
	sbcl \
	--eval '(require :sb-texinfo)' \
	--eval '(require :wo-graph-functions)' \
	--eval '(sb-texinfo:generate-includes "include/" (list :wo-graph-functions) :base-package :wo-graph-functions)' \
	--eval '(quit)'
	touch include-stamp


%.html: %.texinfo style.css include-stamp
	makeinfo --html --no-split --css-include=style.css $<

%.pdf:  %.texinfo include-stamp
	texi2pdf $<

%.info: %.texinfo include-stamp
	makeinfo --no-split $<



%.neato.pdf: %.dot
	neato -Tpdf -o $@ $<

%.dot.pdf: %.dot
	dot -Tpdf -o $@ $<

%.neato.png: %.dot
	neato -Tpng -o $@ $<

%.dot.png: %.dot
	dot -Tpng -o $@ $<



wo-graph-functions.pdf:  $(MAIN_SOURCE) ${IMAGE_SOURCE:.dot=.dot.pdf} include-stamp
wo-graph-functions.html: $(MAIN_SOURCE) ${IMAGE_SOURCE:.dot=.dot.png} include-stamp

html: wo-graph-functions.html
pdf: wo-graph-functions.pdf
info: wo-graph-functions.info

pages: html
	git checkout gh-pages
	cp wo-graph-functions.html ../index.html
	cp $(HTML_IMAGES) ../
	-git commit -a -C master
	rm ../index.html
	git checkout -f master

install-info: info
	cp wo-graph-functions.info ~/share/info/
	install-info wo-graph-functions.info ~/share/info/dir


.PHONY: clean

clean:
	-rm -f *.pdf *.info include-stamp *.toc *.pg *.tp *.vr *.fn *.fns 
	-rm -f *.ky *.cp *.aux *.html *.log graph-*.png
	-rm -rf include/
