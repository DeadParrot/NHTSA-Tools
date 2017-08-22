# NHTSA Library Make File
#
# Language: GNU Make
#
# Platform: Linux/GCC

# Initialization
include $(NHTSA_bin)/GNUmakeinit.mk

# Variables
SRC := $(notdir $(wildcard $(NHTSA)/src/lib/*.for) $(wildcard $(NHTSA)/src/lib/*.f90) $(wildcard $(NHTSA)/src/lib/Linux/*.for) $(wildcard $(NHTSA)/src/lib/Linux/GCC/*.for) $(wildcard $(NHTSA)/src/lib/Linux/GCC/*.f90) $(wildcard $(NHTSA)/src/lib/Linux/GCC/*.c))
DEP := $(addsuffix .d,$(basename $(SRC)))
OBJ := $(addsuffix .o,$(basename $(SRC)))
SLB := $(BIN_PATH)/libNHTSA.a

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
	-rm -f *.d *.o *.mod

.PHONY : clean
clean :
	-rm -f *.d *.o *.mod $(SLB)

# Library from objects
$(SLB) : $(OBJ)
	@-rm -f $(filter-out $(DEP),$(wildcard *.d)) # Prune obs deps
	@-rm -f $(filter-out $(OBJ),$(wildcard *.o)) # Prune obs objs
	$(AR) $(ARFLAGS) $@ $?

# Library from objects member-wise: Slower? More accurate since member date, not library date?
#$(SLB)($(OBJ)) : $(OBJ)
#	@-rm -f $(filter-out $(DEP),$(wildcard *.d)) # Prune obs deps
#	@-rm -f $(filter-out $(OBJ),$(wildcard *.o)) # Prune obs objs
#	$(AR) $(ARFLAGS) $@ $?

# Dependencies
ifneq ($(MAKECMDGOALS),dep)
ifneq ($(MAKECMDGOALS),check)
ifneq ($(MAKECMDGOALS),clear)
ifneq ($(MAKECMDGOALS),clean)
-include $(DEP)
endif
endif
endif
endif
