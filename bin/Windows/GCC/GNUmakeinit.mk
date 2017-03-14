# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Windows/GCC

# Extensions
.SUFFIXES: # Clear default extensions
.SUFFIXES: .for .f90 .fh .fi .c .h .d .o .mod .exe .a .def .dll

# Flags
ARFLAGS := rD

# Commands
FC := gfortran
CC := gcc
MAKEDEPEND := makedep.py --ext=o

# Paths
SRC_PATH := $(NHTSA)\src\app $(NHTSA)\src\lib $(NHTSA)\src\lib\Windows $(NHTSA)\src\lib\Windows\GCC
INC_PATH := $(NHTSA)\src\inc $(NHTSA)\src\inc\Windows $(NHTSA)\src\inc\Windows\GCC
TST_PATH := $(NHTSA)\tst
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
vpath %.def $(BIN_PATH)
vpath %.dll $(BIN_PATH)

# Implicit Rules

%.d : %.for
	@$(MAKEDEPEND) $<

%.d : %.f90
	@$(MAKEDEPEND) $<

%.d : %.c
	@$(MAKEDEPEND) $<

%.o : %.for
	@$(MAKEDEPEND) $<
	$(FC) $(FFLAGS) -c -o $@ $(subst /,\,$<)

%.o : %.f90
	@$(MAKEDEPEND) $<
	$(FC) $(FFLAGS) -c -o $@ $(subst /,\,$<)

%.o : %.c
	@$(MAKEDEPEND) $<
	$(CC) $(CFLAGS) -c -o $@ $(subst /,\,$<)

%.a : %.o
	$(AR) $(ARFLAGS) $@ $?

%.exe : %.o
	$(FC) $(LINKFLAGS) -o $@ $(subst /,\,$<) -lNHTSA -L$(MinGW)\opt\lib -lregex

$(NHTSA_bin)\\%.exe : %.o
	$(FC) $(LINKFLAGS) -o $@ $(subst /,\,$<) -lNHTSA -L$(MinGW)\opt\lib -lregex

# Directives
.DELETE_ON_ERROR : # Delete a target if error occurs during command execution
