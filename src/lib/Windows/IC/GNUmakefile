# NHTSA Library Make File
#
# Language: GNU Make
#
# Platform: Windows/IC

# Initialization
include $(NHTSA_bin)/GNUmakeinit.mk

# Variables
SRC := $(notdir $(wildcard $(NHTSA)/src/lib/*.for) $(wildcard $(NHTSA)/src/lib/*.f90) $(wildcard $(NHTSA)/src/lib/Windows/*.for) $(wildcard $(NHTSA)/src/lib/Windows/IC/*.for) $(wildcard $(NHTSA)/src/lib/Windows/IC/*.f90))
DEP := $(addsuffix .d,$(basename $(SRC)))
OBJ := $(addsuffix .obj,$(basename $(SRC)))
SLB := $(BIN_PATH)\libNHTSA.lib

# Rules

.PHONY : all
all : $(SLB)

.PHONY : lib
lib : $(SLB)

.PHONY : dep
dep : $(DEP)

.PHONY : obj
obj : $(OBJ)

.PHONY : check
check :
	@echo SRC: $(SRC)
	@echo DEP: $(DEP)
	@echo OBJ: $(OBJ)
	@echo SLB: $(SLB)

.PHONY : clear
clear :
	-rm -f *.d *.obj *.mod

.PHONY : clean
clean :
	-rm -f *.d *.obj *.mod $(subst \,/,$(SLB))

# Library from objects: Rebuild whole library ($^ instead of $?): Could do remove then add but fails for new lib
$(SLB) : $(OBJ)
	@-rm -f $(filter-out $(DEP),$(wildcard *.d)) # Prune obs deps
	@-rm -f $(filter-out $(OBJ),$(wildcard *.obj)) # Prune obs objs
	$(AR) $(ARFLAGS) /out:$@ $^

# Library from objects member-wise: Slower? More accurate since member date, not library date?
#$(SLB)($(OBJ)) : $(OBJ)
#	@-rm -f $(filter-out $(DEP),$(wildcard *.d)) # Prune obs deps
#	@-rm -f $(filter-out $(OBJ),$(wildcard *.obj)) # Prune obs objs
#	$(AR) $(ARFLAGS) /out:$@ $?

# Dependencies
ifneq ($(MAKECMDGOALS),dep)
ifneq ($(MAKECMDGOALS),check)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),clean)
-include $(DEP)
endif
endif
endif
endif
