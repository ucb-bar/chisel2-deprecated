# make fragment to build man pages.

LATEX2MAN := latex2man
MAN_PAGES := chisel.man

# Set the current release info
# RELEASE_TAGTEXT is something like: v2.2.18 125 g3501d7f
#  i.e., the output of git describe with dashes replaced by spaces
RELEASE_TAGTEXT=$(subst -, ,$(shell git describe --tags release))
RELEASE_TAG=$(firstword $(RELEASE_TAGTEXT))
RELEASE_DATETEXT=$(shell git log -1 --format="%ai" $(RELEASE_TAG))
RELEASE_DATE=$(firstword $(RELEASE_DATETEXT))

all: $(MAN_PAGES)

%.man: %.mtt
	sed -e "s/@VERSION@/$(RELEASE_TAG)/" -e "s/@DATE@/$(RELEASE_DATE)/" $(notdir $<) > $(basename $@).ttex ;\
	latex2man $(basename $@).ttex $@

