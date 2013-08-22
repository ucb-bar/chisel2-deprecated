host=psi.millennium.berkeley.edu
path=/project/eecs/parlab/www/chisel/data
parlaball_grpid=683

all: index.html documentation.html download.html support.html

install:
	rsync -rlvzC --delete-after ./* $(host):$(path)/
	-ssh $(host) chgrp -fR $(parlaball_grpid) $(path)/\*
	-ssh $(host) chmod -fR 775 $(path)/\*

%.html: ../doc/templates/%.html
	../bin/jinja2html.py $(notdir $<) $@
