# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Windows/IC/64/r

# Variables
FFLAGS := /nologo /warn /warn:nodeclarations /warn:nounused /warn:nointerfaces /nogen-interfaces /assume:noold_unit_star,dummy-aliases /debug:none /O3 /Qip /DNDEBUG
LDFLAGS := /nologo /STACK:8388608
LINKFLAGS := /link /LIBPATH:$(NHTSA_bin)

include $(NHTSA_bin)\..\GNUmakeinit.mk
