host=psi.millennium.berkeley.edu
path=/project/eecs/parlab/www/chisel/data
parlaball_grpid=683

install:
	rsync -rlvz --delete-after ./* $(host):$(path)/
	-ssh $(host) chgrp -fR $(parlaball_grpid) $(path)/\*
	-ssh $(host) chmod -fR 775 $(path)/\*
