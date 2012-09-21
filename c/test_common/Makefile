BASE_DIR=..

LIBNAME = test_common

INCLUDE +=-I$(PWD) -I$(BASE_DIR)/common

SRC = test_struct.c
SRC += rudimentary_mem_manager.c

LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OUTDIR) -lcommon

include ../build/GlobalMake
