basedir = ..
SBT = java -Xmx1024M -Xss8M -jar sbt-launch.jar

tutorials = \
	gcd \
	combinational \
	functional \
	mux2 \
	mux4 \
	sequential \
	parity \
	memo \
	filter \
	tbl \
	life \

tut_out = $(addsuffix .out, $(tutorials))

$(tutorials):
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $@ --compile --test"

$(tut_out): %.out: ${CHISEL}/src/*.scala ../src/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $* --compile --test" > $(basedir)/emulator/$@

all: $(tut_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /(PASSED|FAILED)/' $(tut_out); echo

clean: 
	rm -f *.out *.o