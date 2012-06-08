basedir = ..
include ../Makefrag
SBTARGS = --backend c --compile --test --targetDir ../emulator --genHarness

tut_out = $(addsuffix .out, $(tutorials))

$(tutorials): %: ${CHISEL}/src/*.scala ../src/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $@ $(SBTARGS)"

$(tut_out): %.out: ${CHISEL}/src/*.scala ../src/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $* $(SBTARGS)" > $(basedir)/emulator/$@

all: $(tut_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /(PASSED|FAILED)/' $(tut_out); echo

clean: 
	rm -f *.out *.o $(tutorials) $(addsuffix .h, $(tutorials)) $(addsuffix .cpp, $(tutorials))
