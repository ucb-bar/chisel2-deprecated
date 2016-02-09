# Retain all intermediate files.
.SECONDARY:

SBT		?= sbt
SBT_FLAGS	?= -Dsbt.log.noformat=true
RM_DIRS 	:= test-outputs test-reports
CLEAN_DIRS	:= doc

SRC_DIR	?= .
#SYSTEMC ?= $(SRC_DIR)/../../systemc/systemc-2.3.1
SYSCTESTS ?= $(addsuffix .sysctest,$(notdir $(basename $(wildcard $(SRC_DIR)/src/test/scala/SysCTest/*.scala))))
CHISEL_JAR ?= $(SRC_DIR)/target/scala-2.10/chisel_2.10-2.3-SNAPSHOT.jar
TEST_OUTPUT_DIR ?= ./test-outputs

.PHONY:	smoke publish-local check clean jenkins-build sysctest coverage scaladoc test compile style

SMOKE_TESTS ?= StdlibSuite

default:	publish-local

compile:
	$(SBT) $(SBT_FLAGS) compile

publish-local:
	$(SBT) $(SBT_FLAGS) publish-local

smoke:
	$(SBT) $(SBT_FLAGS) "test-only $(SMOKE_TESTS) -- -l org.scalatest.tags.Slow"

style:
	$(SBT) $(SBT_FLAGS) scalastyle test:scalastyle

check test:
	$(SBT) $(SBT_FLAGS) test

coverage:
	$(SBT) $(SBT_FLAGS) coverage test
	$(SBT) $(SBT_FLAGS) coverageReport

clean:
	$(SBT) $(SBT_FLAGS) +clean
	for dir in $(CLEAN_DIRS); do $(MAKE) -C $$dir clean; done
	$(RM) -r $(RM_DIRS)

scaladoc:
	$(SBT) $(SBT_FLAGS) doc test:doc

# Start off clean, then run tests for all supported configurations, and publish those versions of the code.
# Then run coverage and style tests (for developer's use).
# Don't publish the coverage test code since it contains hooks/references to the coverage test package
# and we don't want code with those dependencies published.
# We need to run the coverage tests last, since Jenkins will fail the build if it can't find their results.
jenkins-build: clean
	$(SBT) $(SBT_FLAGS) +test
	$(SBT) $(SBT_FLAGS) +clean +publish-local
	$(SBT) $(SBT_FLAGS) scalastyle coverage test
	$(SBT) $(SBT_FLAGS) coverageReport

.PHONY:	SYSCDIR

SYSCDIR:
	@if [ -z "$(SYSTEMC)" ]; then echo "Please define SYSTEMC (the root of the systemc distribution) in your environment"; exit 1; fi
	@if [ ! -d "$(SYSTEMC)" ]; then echo "SYSTEMC isn't a valid directory - $(SYSTEMC)"; exit 1; fi

sysctests: $(SYSCTESTS) SYSCDIR

sysctest:  $(firstword $(SYSCTESTS)) SYSCDIR

%.sysctest:
	mkdir -p $(TEST_OUTPUT_DIR)
	$(MAKE) -C $(TEST_OUTPUT_DIR) -f ../Makefile SRC_DIR=.. $(basename $@).sysc
	cd $(TEST_OUTPUT_DIR) && ./$(basename $@).sysc

%.sysc:	%.h %.cpp $(SRC_DIR)/src/test/resources/%SysCdriver.cpp
	$(CXX) -g $(filter-out %.h,$^) \
	   -I. -I$(SYSTEMC)/include -L$(SYSTEMC)/lib-macosx64 -lsystemc -o $@

%.h %.cpp:	%.class
	scala -cp $(CHISEL_JAR):. SysCTest.$(basename $@) --targetDir . --genHarness --backend sysc

%.class:  ../src/test/scala/SysCTest/%.scala $(CHISEL_JAR)
	scalac -cp $(CHISEL_JAR) $<

