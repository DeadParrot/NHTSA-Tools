# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Windows/GCC/64/d

# Variables
FFLAGS := -pipe -Wall -Wno-unused -Wno-unused-dummy-argument -Wno-character-truncation -m64 -mtune=generic -fdec-structure -fno-sign-zero -ffree-line-length-0 -x f95-cpp-input -ffloat-store -fcheck=bounds -fbacktrace -ftrapv -finit-integer=-999999 -finit-real=nan -O0 -ggdb -I$(NHTSA)\src\inc\Windows\GCC -I$(NHTSA)\src\inc\Windows -I$(NHTSA)\src\inc
CFLAGS := -pipe -std=c99 -Wall -m64 -mtune=generic -O0 -ggdb -I$(NHTSA)\src\inc\Windows\GCC -I$(NHTSA)\src\inc\Windows -I$(NHTSA)\src\inc
LINKFLAGS := -pipe -m64 -mtune=generic -O0 -ggdb
LDFLAGS := -pipe -Wall -ggdb

include $(NHTSA_bin)\..\GNUmakeinit.mk
