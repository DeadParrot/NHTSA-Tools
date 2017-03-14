# NHTSA Applications Make File
#
# Language: GNU Make
#
# Platform: Windows\IC
#
# Compiler: Intel Fortran

# Initialization
include $(NHTSA_bin)/GNUmakeinit.mk

# Variables
SRC := $(notdir $(wildcard $(NHTSA)/src/app.new/*.f90))
DEP := $(addsuffix .d,$(basename $(SRC)))
OBJ := $(addsuffix .obj,$(basename $(SRC)))
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
	-rm -f *.d *.obj *.mod

.PHONY : clean
clean :
	-rm -f *.d *.obj *.mod $(subst \,/,$(APP))

appname.obj : appname.f90 # Prevent make from picking up .for version
	@$(MAKEDEPEND) $<
	$(FC) $(FFLAGS) /c $(subst /,\,$<)

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
