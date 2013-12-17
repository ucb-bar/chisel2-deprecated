# Building the docs on osx will require to install Jinja2 and BeautifulSoup:
#     $ pip install Jinja2 BeautifulSoup
# and the different tools for text to pdf:
#     $ port install texlive-latex-extra texlive-latex-recommended \
#           texlive-htmlxml ImageMagick

version   := 2.1

PDFLATEX  := pdflatex
WWW_PAGES := index.html documentation.html download.html faq.html
WWW_EXTRA := manual.html getting-started.html # tutorial.html cs250-1.html cs250-2.html cs250-3.html
PDFS      := $(addsuffix .pdf,$(addprefix chisel-,\
             cs250-1 cs250-2 cs250-3 \
             dac12-talk manual tutorial getting-started \
             bootcamp installation))
srcDir    := .
installTop:= ../www

vpath %.tex $(srcDir)/bootcamp $(srcDir)/cs250 $(srcDir)/installation $(srcDir)/talks/dac12 $(srcDir)/manual $(srcDir)/tutorial $(srcDir)/getting-started

all: $(WWW_PAGES) $(WWW_EXTRA) $(PDFS)

extra: $(WWW_EXTRA)

html: $(WWW_PAGES)

pdf: $(PDFS)

install: all
	install -d $(installTop)/$(version)/figs
	install -m 644 $(wildcard $(srcDir)/cs250/figs/*.png $(srcDir)/manual/figs/*.png $(srcDir)/tutorial/figs/*.png) $(installTop)/$(version)/figs
	install -m 644 $(WWW_EXTRA) $(PDFS) $(installTop)/$(version)
	install -m 644 $(WWW_PAGES) $(installTop)


chisel-%.pdf: %.tex
	cd $(dir $<) && TEXINPUTS=".:$(PWD)/$(srcDir)/manual:${TEXINPUTS}" pdflatex -file-line-error -interaction nonstopmode -output-directory $(PWD) $(notdir $<)
	mv $(subst .tex,.pdf,$(notdir $<)) $@

%.html: %.tex
	cd $(dir $<) && TEXINPUTS=".:$(PWD)/$(srcDir)/manual:${TEXINPUTS}" htlatex $(notdir $<) $(PWD)/$(srcDir)/html.cfg "" -d/$(PWD)/
	mv $(subst .tex,.html,$(notdir $<)) $@~
	$(srcDir)/../bin/tex2html.py $@~ $@

%.html: $(srcDir)/templates/%.html
	$(srcDir)/../bin/jinja2html.py $(notdir $<) $@

clean:
	-rm -f $(addprefix manual/,*.4ct *.4tc *.css *.dvi *.html *.idv *.lg *.tmp *.xref)
	-rm -f $(addprefix manual/figs/,bits-1.png bits-and.png bits-or-and.png node-hierarchy.png type-hierarchy.png)
	-rm -f $(addprefix tutorial/,*.4ct *.4tc *.css *.dvi *.html *.idv *.lg *.tmp *.xref)
	-rm -f $(addprefix tutorial/figs/,DUT.png DUT.svg condupdates.png)
	-rm -f $(addprefix getting-started/,*.4ct *.4tc *.css *.dvi *.html *.idv *.lg *.tmp *.xref) getting-started?.html
	-rm -f $(addprefix cs250/,*.4ct *.4tc *.css *.dvi *.html *.idv *.lg *.tmp *.xref)
	-rm -f $(WWW_PAGES) $(PDFS) $(WWW_EXTRA) $(addsuffix .1,$(WWW_EXTRA)) $(patsubst %.html,%.css,$(WWW_EXTRA))
	-rm -f *~ *.aux *.log *.nav *.out *.snm *.toc *.vrb
	-rm -f *.jpg *.png

