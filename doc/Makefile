.PHONE: clean


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



html: wo-graph-functions.html

pdf:  wo-graph-functions.pdf

info: wo-graph-functions.info


pages: html
	git checkout gh-pages
	cp wo-graph-functions.html ../index.html
	git commit -a -C master
	rm ../index.html
	git checkout -f master

install-info: info
	cp wo-graph-functions.info ~/share/info/
	install-info wo-graph-functions.info ~/share/info/dir
