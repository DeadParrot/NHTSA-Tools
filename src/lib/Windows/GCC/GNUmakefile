# NHTSA Library Make File
#
# Language: GNU Make
#
# Platform: Windows/GCC

# Initialization
include $(NHTSA_bin)/GNUmakeinit.mk

# Variables
SRC := $(notdir $(wildcard $(NHTSA)/src/lib/*.for) $(wildcard $(NHTSA)/src/lib/*.f90) $(wildcard $(NHTSA)/src/lib/Windows/*.for) $(wildcard $(NHTSA)/src/lib/Windows/GCC/*.for) $(wildcard $(NHTSA)/src/lib/Windows/GCC/*.f90) $(wildcard $(NHTSA)/src/lib/Windows/GCC/*.c))
DEP := $(addsuffix .d,$(basename $(SRC)))
OBJ := $(addsuffix .o,$(basename $(SRC)))
SLB := $(BIN_PATH)\libNHTSA.a

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
	-DEL *.d *.o *.mod

.PHONY : clean
clean :
	-DEL *.d *.o *.mod $(SLB)

# Library from objects
$(SLB) : $(OBJ)
	@-FOR %%F in ($(filter-out $(DEP),$(wildcard *.d))) DO DEL %%F
	@-FOR %%F in ($(filter-out $(OBJ),$(wildcard *.o))) DO DEL %%F
	$(AR) $(ARFLAGS) $@ $?

# Library from objects member-wise: Slower? More accurate since member date, not library date?
#$(SLB)($(OBJ)) : $(OBJ)
#	@-FOR %%F in ($(filter-out $(DEP),$(wildcard *.d))) DO DEL %%F
#	@-FOR %%F in ($(filter-out $(OBJ),$(wildcard *.o))) DO DEL %%F
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
