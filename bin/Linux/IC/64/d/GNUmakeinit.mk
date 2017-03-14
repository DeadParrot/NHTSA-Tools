# NHTSA Make Initialization File
#
# Language: GNU Make
#
# Platform: Linux/IC/64/d

# Variables
FFLAGS := <fill this in>
ifneq ( $(wildcard /usr/lib64/gfortran/modules), )
  FFLAGS += -I/usr/lib64/gfortran/modules
else
  ifneq ( $(wildcard /usr/lib/gfortran/modules), )
    FFLAGS += -I/usr/lib/gfortran/modules
  endif
endif
LDFLAGS := -pipe -Wall -g

include $(NHTSA_bin)/../GNUmakeinit.mk
