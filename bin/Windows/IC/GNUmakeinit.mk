# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Windows/IC

# Extensions
.SUFFIXES: # Clear default extensions
.SUFFIXES: .for .f90 .fh .fi .d .obj .mod .exe .lib .def .dll

# Flags
ARFLAGS := /nologo

# Commands
FC := ifort
AR := lib
LD := link
MAKEDEPEND := makedep.py --inc=INCLUDE

# Paths
SRC_PATH := $(NHTSA)\src\app $(NHTSA)\src\lib $(NHTSA)\src\lib\Windows $(NHTSA)\src\lib\Windows\IC
INC_PATH := $(NHTSA)\src\inc $(NHTSA)\src\inc\Windows $(NHTSA)\src\inc\Windows\IC
TST_PATH := $(NHTSA)\tst
OBJ_PATH := .
MOD_PATH := .
BIN_PATH := $(NHTSA_bin)

# Search Paths
vpath %.for $(SRC_PATH)
vpath %.f90 $(SRC_PATH)
vpath %.fh  $(INC_PATH)
vpath %.fi  $(INC_PATH)
vpath %.d   $(OBJ_PATH)
vpath %.obj $(OBJ_PATH)
vpath %.mod $(MOD_PATH)
vpath %.exe $(BIN_PATH)
vpath %.lib $(BIN_PATH)
vpath %.def $(BIN_PATH)
vpath %.dll $(BIN_PATH)

# Implicit Rules

%.d : %.for
	@$(MAKEDEPEND) $<

%.d : %.f90
	@$(MAKEDEPEND) $<

%.obj : %.for
	@$(MAKEDEPEND) $<
	$(FC) $(FFLAGS) /c $(subst /,\,$<)

%.obj : %.f90
	@$(MAKEDEPEND) $<
	$(FC) $(FFLAGS) /c $(subst /,\,$<)

# Library from objects: Rebuild whole library ($^ instead of $?): Could do remove then add but fails for new lib
%.lib : %.obj
	$(AR) $(ARFLAGS) /out:$@ $^

%.exe : %.obj
	$(FC) $(FFLAGS) /F0x800000 $(subst /,\,$<) libNHTSA.lib /exe:$@ /link

$(NHTSA_bin)\\%.exe : %.obj
	$(FC) $(FFLAGS) /F0x800000 $(subst /,\,$<) libNHTSA.lib /exe:$@ /link

# Directives
.DELETE_ON_ERROR : # Delete a target if error occurs during command execution
