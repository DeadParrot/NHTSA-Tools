# NHTSA Applications Make File
#
# Language: GNU Make
#
# Platform: Windows/GCC

# Initialization
include $(NHTSA_bin)/GNUmakeinit.mk

# Variables
SRC := $(notdir $(wildcard $(NHTSA)/src/app/*.for) $(wildcard $(NHTSA)/src/app/*.f90))
DEP := $(addsuffix .d,$(basename $(SRC)))
OBJ := $(addsuffix .o,$(basename $(SRC)))
APP := $(addprefix $(NHTSA_bin)\,$(addsuffix .exe,$(basename $(SRC))))

# Rules

.PHONY : all
all : $(APP)

.PHONY : app
app : $(APP)

.PHONY : dep
dep : $(DEP)

.PHONY : obj
obj : $(OBJ)

.PHONY : check
check :
	@echo SRC: $(SRC)
	@echo DEP: $(DEP)
	@echo OBJ: $(OBJ)
	@echo APP: $(APP)

.PHONY : clear
clear :
	-rm -f *.d *.o *.mod

.PHONY : clean
clean :
	-rm -f *.d *.o *.mod $(subst \,/,$(APP))

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
