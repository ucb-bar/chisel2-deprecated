# Building the docs on osx will require to install Jinja2:
#     $ pip install Jinja2
# and the different tools for text to pdf:
#     $ port install texlive-latex-extra texlive-latex-recommended \
#           texlive-htmlxml ImageMagick

host=psi.millennium.berkeley.edu
path=/project/eecs/parlab/www/chisel/data
parlaball_grpid=683

PDFLATEX  := pdflatex
WWW_PAGES := index.html documentation.html download.html faq.html
PDFS      := $(addsuffix .pdf,$(addprefix chisel-,\
             cs250-1 cs250-2 cs250-3 \
             dac12-talk manual tutorial \
             bootcamp installation))

vpath %.tex ../doc/bootcamp ../doc/cs250 ../doc/installation ../doc/talks/dac12 ../doc/manual ../doc/tutorial

all: $(WWW_PAGES) $(PDFS)

install:
	rsync -rlvzC --delete-after ./* $(host):$(path)/
	-ssh $(host) chgrp -fR $(parlaball_grpid) $(path)/\*
	-ssh $(host) chmod -fR 775 $(path)/\*

%.html: ../doc/templates/%.html
	../bin/jinja2html.py $(notdir $<) $@

chisel-%.pdf: %.tex
	cd $(dir $<) && TEXINPUTS=".:$(PWD)/../doc/manual:${TEXINPUTS}" pdflatex -file-line-error -interaction nonstopmode -output-directory $(PWD) $(notdir $<)
	mv $(subst .tex,.pdf,$(notdir $<)) $@

chisel-%.html: %.tex
	cd $(dir $<) && TEXINPUTS=".:$(PWD)/../doc/manual:${TEXINPUTS}" htlatex $(notdir $<) "" "" -d/$(PWD)/

clean:
	-rm -f $(WWW_PAGES) $(PDFS) *.aux *.log *.nav *.out *.snm *.toc *.vrb


