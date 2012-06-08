basedir = ..
include ../Makefrag
SBTARGS = --backend v --targetDir ../verilog --genHarness

tut_v = $(addsuffix .v, $(tutorials))

$(tut_v): %.v: ${CHISEL}/src/*.scala ../src/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $* $(SBTARGS)" > $(basedir)/verilog/$@

all: $(tut_v)

clean: 
	rm -f *.out *.o $(tutorials) $(addsuffix .v, $(tutorials))
