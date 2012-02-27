# CPPFLAGS = -O2  #  -S -g
CPPFLAGS = -g -O2

all: tests

tests.o: tests.cpp emulator.h
	g++ $(CPPFLAGS) -c -g tests.cpp
tests: tests.o 
	g++ $(CPPFLAGS) -o tests -g tests.o
clean:
	rm -f *.o emulator test
