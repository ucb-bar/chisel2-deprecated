host=psi.millennium.berkeley.edu
path=/project/eecs/parlab/www/chisel/data
parlaball_grpid=683

install:
	rsync -avz --delete-after ./* $(host):$(path)/
	ssh $(host) chgrp -R $(parlaball_grpid) $(path)/\*
	ssh $(host) chmod -R 775 $(path)/\*
