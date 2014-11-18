# Building the docs on osx will require to install Jinja2 and BeautifulSoup:
#     $ pip install Jinja2 BeautifulSoup
# and the different tools for text to pdf:
#     $ port install texlive-latex-extra texlive-latex-recommended \
#           texlive-htmlxml ImageMagick

version   := 2.2.0

PDFLATEX  := pdflatex
WWW_PAGES := index.html documentation.html download.html faq.html releases.html
WWW_EXTRA := manual.html getting-started.html

# The following subdirectories build documentation correctly.
PDF_DIRS	:= installation manual tutorial getting-started dac12-talk
PDFS      := $(addsuffix .pdf,$(addprefix chisel-,$(PDF_DIRS)))	

LATEX2MAN := latex2man
MAN_PAGES := chisel.man

srcDir    := .
installTop:= ../www

# Set the current release info
# RELEASE_TAGTEXT is something like: v2.2.18 125 g3501d7f
#  i.e., the output of git describe with dashes replaced by spaces
RELEASE_TAGTEXT=$(subst -, ,$(shell git describe --tags release))
RELEASE_TAG=$(firstword $(RELEASE_TAGTEXT))
RELEASE_DATETEXT=$(shell git log -1 --format="%ai" $(RELEASE_TAG))
RELEASE_DATE=$(firstword $(RELEASE_DATETEXT))

vpath %.tex $(srcDir)/bootcamp $(srcDir)/installation $(srcDir)/talks/dac12 $(srcDir)/manual $(srcDir)/tutorial $(srcDir)/getting-started

vpath %.mtt $(srcDir)/bootcamp $(srcDir)/installation $(srcDir)/talks/dac12 $(srcDir)/manual $(srcDir)/tutorial $(srcDir)/getting-started

all: $(WWW_PAGES) $(WWW_EXTRA) $(PDFS)

extra: $(WWW_EXTRA)

html: $(WWW_PAGES)

pdf: $(PDFS)

install: all
	install -d $(installTop)/$(version)/figs
	install -m 644 $(wildcard $(srcDir)/manual/figs/*.png $(srcDir)/tutorial/figs/*.png) $(installTop)/$(version)/figs
	install -m 644 $(WWW_EXTRA) $(PDFS) $(installTop)/$(version)
	install -m 644 $(WWW_PAGES) $(installTop)


chisel-%.pdf: %.tex
	cd $(dir $<) && TEXINPUTS=".:$(PWD)/$(srcDir)/manual:${TEXINPUTS}" pdflatex -file-line-error -interaction nonstopmode -output-directory $(PWD) $(notdir $<)
	mv $(subst .tex,.pdf,$(notdir $<)) $@

%.html: %.tex
	cd $(dir $<) && TEXINPUTS=".:$(PWD)/$(srcDir)/manual:${TEXINPUTS}" htlatex $(notdir $<) $(PWD)/$(srcDir)/html.cfg "" -d/$(PWD)/
	mv $(subst .tex,.html,$(notdir $<)) $@~
	$(srcDir)/../bin/tex2html.py $@~ $@

%.man: %.mtt
	# cd into the directory containing the .tex file and massage it
	cd $(dir $<) && \
	sed -e "s/@VERSION@/$(RELEASE_TAG)/" -e "s/@DATE@/$(RELEASE_DATE)/" $(notdir $<) > $(basename $@).ttex ;\
	TEXINPUTS=".:$(PWD)/$(srcDir)/manual:${TEXINPUTS}" latex2man $(basename $@).ttex $@

%.html: $(srcDir)/templates/%.html $(srcDir)/templates/base.html
	$(srcDir)/../bin/jinja2html.py $(notdir $<) $@

releases.html:	$(srcDir)/templates/releases.html $(srcDir)/templates/base.html
	sed -e "s/@VERSION@/$(RELEASE_TAG)/" -e "s/@DATE@/$(RELEASE_DATE)/" $< > $(dir $<)/$@.tmp
	$(srcDir)/../bin/jinja2html.py $@.tmp $@ && ${RM} $(dir $<)/$@.tmp

clean:
	-rm -f $(addprefix manual/,*.4ct *.4tc *.css *.dvi *.html *.idv *.lg *.tmp *.xref)
	-rm -f $(addprefix manual/figs/,bits-1.png bits-and.png bits-or-and.png node-hierarchy.png type-hierarchy.png)
	-rm -f $(addprefix tutorial/,*.4ct *.4tc *.css *.dvi *.html *.idv *.lg *.tmp *.xref)
	-rm -f $(addprefix tutorial/figs/,DUT.png DUT.svg condupdates.png)
	-rm -f $(addprefix getting-started/,*.4ct *.4tc *.css *.dvi *.html *.idv *.lg *.tmp *.xref) getting-started?.html
	-rm -f $(WWW_PAGES) $(PDFS) $(WWW_EXTRA) $(addsuffix .1,$(WWW_EXTRA)) $(patsubst %.html,%.css,$(WWW_EXTRA))
	-rm -f *~ *.aux *.log *.nav *.out *.snm *.toc *.vrb
	-rm -f *.jpg *.png
	-rm -f manual/chisel.man manual/chisel.ttex

