# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Windows/GCC/64/r

# Variables
FFLAGS := -pipe -Wall -Wno-unused -Wno-unused-dummy-argument -Wno-character-truncation -m64 -mtune=generic -fdec-structure -fno-sign-zero -ffree-line-length-0 -x f95-cpp-input -DNDEBUG -O2 -s -I$(NHTSA)\src\inc\Windows\GCC -I$(NHTSA)\src\inc\Windows -I$(NHTSA)\src\inc
CFLAGS := -pipe -std=c99 -Wall -m64 -mtune=generic -DNDEBUG -O2 -s -I$(NHTSA)\src\inc\Windows\GCC -I$(NHTSA)\src\inc\Windows -I$(NHTSA)\src\inc
LINKFLAGS := -pipe -m64 -mtune=generic -DNDEBUG -O2 -s
LDFLAGS := -pipe -Wall -s

include $(NHTSA_bin)\..\GNUmakeinit.mk
