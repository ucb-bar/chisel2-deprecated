basedir = ..
SBT = java -Xmx1024M -Xss8M -jar sbt-launch.jar

tutorials = \
	GCD \
	Combinational \
	Functional \
	Mux2 \
	Mux4 \
	Accumulator \
	Parity \
	Memo \
	Filter \
	Tbl \
	Life \
	Mul \
	Echo \
	Risc \

tut_out = $(addsuffix .out, $(tutorials))

$(tutorials): %: ${CHISEL}/src/*.scala ../src/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $@ --compile --test"

$(tut_out): %.out: ${CHISEL}/src/*.scala ../src/%.scala
	cd $(basedir)/sbt; $(SBT) "project tutorial" "run $* --compile --test" > $(basedir)/emulator/$@

all: $(tut_out)
	@echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /(PASSED|FAILED)/' $(tut_out); echo

clean: 
	rm -f *.out *.o $(tutorials) $(addsuffix .h, $(tutorials)) $(addsuffix .cpp, $(tutorials))
