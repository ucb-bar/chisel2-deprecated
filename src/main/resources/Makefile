# Chisel parallel make template.

HFILES   = @HFILES@
UNOPTIMIZEDO0 = @UNOPTIMIZEDO0@
UNOPTIMIZEDO1 = @UNOPTIMIZEDO1@
OPTIMIZED = @OPTIMIZED@

EXEC    = @EXEC@

CPPFLAGS  = @CPPFLAGS@

default:  $(EXEC)

clean:
	$(RM) $(EXEC) $(UNOPTIMIZEDO0) $(UNOPTIMIZEDO1) $(OPTIMIZED)

$(UNOPTIMIZEDO0) $(UNOPTIMIZEDO1) $(OPTIMIZED):  $(HFILES)

$(EXEC):  $(UNOPTIMIZEDO0) $(UNOPTIMIZEDO1) $(OPTIMIZED) Makefile
	$(CXX) -o $@ $(filter-out Makefile,$^)

$(OPTIMIZED): %.o: %.cpp
	$(CXX) -c -o $@ -O2 $(CPPFLAGS) $(CXXFLAGS) $<

$(UNOPTIMIZEDO0): %.o: %.cpp
	$(CXX) -c -o $@ -O0 $(CPPFLAGS) $(CXXFLAGS) $<

$(UNOPTIMIZEDO1): %.o: %.cpp
	$(CXX) -c -o $@ -O1 $(CPPFLAGS) $(CXXFLAGS) $<
