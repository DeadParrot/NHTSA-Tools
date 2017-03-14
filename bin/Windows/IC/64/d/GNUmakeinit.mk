# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Windows/IC/64/d

# Variables
FFLAGS := /nologo /warn /warn:nodeclarations /warn:nounused /warn:nointerfaces /nogen-interfaces /assume:noold_unit_star,dummy-aliases /Z7 /Od /fp:precise /check:all /traceback /Gs0 /Qfp-stack-check /Qinit:arrays /Qinit:snan
LDFLAGS := /nologo /STACK:8388608 /DEBUG
LINKFLAGS := /link /LIBPATH:$(NHTSA_bin) /DEBUG

include $(NHTSA_bin)\..\GNUmakeinit.mk
