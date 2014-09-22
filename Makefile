SBT		?= sbt
SBT_FLAGS	?= -Dsbt.log.noformat=true
RM_DIRS 	:= test-outputs test-reports
CLEAN_DIRS	:= doc

SRC_DIR	?= .
SYSTEMC ?= $(SRC_DIR)/../../systemc/systemc-2.3.1
CHISEL_JAR ?= $(SRC_DIR)/target/scala-2.10/chisel_2.10-2.3-SNAPSHOT.jar
DRIVER	   ?= $(SRC_DIR)/src/test/resources/AddFilterSysCdriver.cpp
TEST_OUTPUT_DIR ?= ./test-outputs

.PHONY:	smoke publish-local check clean jenkins-build sysctest

default:	publish-local

smoke:
	$(SBT) $(SBT_FLAGS) compile

publish-local:
	$(SBT) $(SBT_FLAGS) publish-local

check test:
	$(SBT) $(SBT_FLAGS) scct:test

clean:
	$(SBT) $(SBT_FLAGS) clean
	for dir in $(CLEAN_DIRS); do $(MAKE) -C $$dir clean; done
	$(RM) -r $(RM_DIRS)

jenkins-build:
	$(SBT) $(SBT_FLAGS) clean scalastyle scct:test publish-local

sysctest:
	mkdir -p $(TEST_OUTPUT_DIR)
	$(MAKE) -C $(TEST_OUTPUT_DIR) -f ../Makefile SRC_DIR=.. syscbuildandruntest

syscbuildandruntest:	AddFilter
	./AddFilter

AddFilter:	AddFilter.h AddFilter.cpp $(SYSC_DRIVER)
	$(CXX)  AddFilter.cpp $(DRIVER) \
	   -I. -I$(SYSTEMC)/include -L$(SYSTEMC)/lib-macosx64 -lsystemc -o $@

AddFilter.cpp AddFilter.h:	   AddFilter.class
	scala -cp $(CHISEL_JAR):. AddFilter --targetDir . --genHarness --backend sysc --design AddFilter

AddFilter.class:  $(CHISEL_JAR) ../src/test/scala/AddFilter.scala
	scalac -cp $(CHISEL_JAR) ../src/test/scala/AddFilter.scala

