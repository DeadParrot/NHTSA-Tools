# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Linux/GCC

# Extensions
.SUFFIXES: # Clear default extensions
.SUFFIXES: .for .f90 .fh .fi .c .h .d .o .mod .exe .a .so

# Flags
ARFLAGS := rD

# Commands
FC := gfortran
CC := gcc
MAKEDEPEND := makedep.py

# Paths
SRC_PATH := $(NHTSA)/src/app $(NHTSA)/src/lib $(NHTSA)/src/lib/Linux $(NHTSA)/src/lib/Linux/GCC
INC_PATH := $(NHTSA)/src/inc $(NHTSA)/src/inc/Linux $(NHTSA)/src/inc/Linux/GCC
TST_PATH := $(NHTSA)/tst
OBJ_PATH := .
MOD_PATH := .
BIN_PATH := $(NHTSA_bin)

# Search Paths
vpath %.for $(SRC_PATH)
vpath %.f90 $(SRC_PATH)
vpath %.c   $(SRC_PATH)
vpath %.fh  $(INC_PATH)
vpath %.fi  $(INC_PATH)
vpath %.h   $(INC_PATH)
vpath %.d   $(OBJ_PATH)
vpath %.o   $(OBJ_PATH)
vpath %.mod $(MOD_PATH)
vpath %.exe $(BIN_PATH)
vpath %.a   $(BIN_PATH)
vpath %.so  $(BIN_PATH)

# Implicit Rules

%.d : %.for
	@$(MAKEDEPEND) $<

%.d : %.f90
	@$(MAKEDEPEND) $<

%.d : %.c
	@$(MAKEDEPEND) $<

%.o : %.for
	@$(MAKEDEPEND) $<
	$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.f90
	@$(MAKEDEPEND) $<
	$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.c
	@$(MAKEDEPEND) $<
	$(CC) $(CFLAGS) -c -o $@ $<

%.lib : %.o
	$(AR) $(ARFLAGS) $@ $?

% : %.o
	$(FC) $(LINKFLAGS) -o $@ $< -lNHTSA

$(NHTSA_bin)/% : %.o
	$(FC) $(LINKFLAGS) -o $@ $< -lNHTSA

# Directives
.DELETE_ON_ERROR : # Delete a target if error occurs during command execution
