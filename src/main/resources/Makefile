# Chisel parallel make template.

HFILES   = @HFILES@
ONCEONLY = @ONCEONLY@
UNOPTIMIZED = @UNOPTIMIZED@
OPTIMIZED = @OPTIMIZED@

EXEC    = @EXEC@
OPTIM0  = @OPTIM0@
OPTIM1	= @OPTIM1@
OPTIM2	= @OPTIM2@
CPPFLAGS  = @CPPFLAGS@
CXXFLAGS  = @CXXFLAGS@
LDFLAGS  = @LDFLAGS@
CXX			= @CXX@

default:  $(EXEC)

clean:
	$(RM) $(EXEC) $(ONCEONLY) $(UNOPTIMIZED) $(OPTIMIZED)

$(ONCEONLY) $(UNOPTIMIZED) $(OPTIMIZED):  $(HFILES)

$(EXEC):  $(ONCEONLY) $(UNOPTIMIZED) $(OPTIMIZED) Makefile
	$(CXX) -o $@ $(filter-out Makefile,$^)

ifneq (,$(ONCEONLY))
$(ONCEONLY): %.o: %.cpp
	$(CXX) -c -o $@ $(OPTIM0) $(CPPFLAGS) $(CXXFLAGS) $<
endif

ifneq (,$(UNOPTIMIZED))
$(UNOPTIMIZED): %.o: %.cpp
	$(CXX) -c -o $@ $(OPTIM1) $(CPPFLAGS) $(CXXFLAGS) $<
endif

ifneq (,$(OPTIMIZED))
$(OPTIMIZED): %.o: %.cpp
	$(CXX) -c -o $@ $(OPTIM2) $(CPPFLAGS) $(CXXFLAGS) $<
endif
