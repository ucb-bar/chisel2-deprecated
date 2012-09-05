basedir = ..
include ../Makefrag
SBTARGS = --backend v --targetDir ../verilog --genHarness --compile --test

tut_v = $(addsuffix .v, $(tutorials))
tut_out = $(addsuffix .out, $(tutorials))

all: $(tut_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /(PASSED|FAILED)/' $(tut_out); echo

$(tutorials): %: ../src/main/scala/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $@ $(SBTARGS)"

$(tut_out): %.out: ../src/main/scala/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $* $(SBTARGS)" > $(basedir)/verilog/$@

clean: 
	rm -rf *.out *.o $(tutorials) $(addsuffix .v, $(tutorials)) *.conf *~ *.daidir

.PHONY: all clean
