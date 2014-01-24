# CPPFLAGS = -O2  #  -S -g
CPPFLAGS = -g -O2 -std=c++11 

all: tests

tests.o: tests.cpp emulator.h
	g++-4.8 $(CPPFLAGS) -c -g tests.cpp
tests: tests.o 
	g++-4.8 $(CPPFLAGS) -o tests -g tests.o
clean:
	rm -f *.o emulator test
