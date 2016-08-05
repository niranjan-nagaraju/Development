BASE_DIR=..

LIBNAME = test_common

INCLUDE +=-I./include -I$(BASE_DIR)/common

LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include ../build/Makefile.inc
