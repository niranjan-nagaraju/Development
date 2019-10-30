BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD)/include -I$(BASE_DIR)/common/include

# Library filenames, _MT for multi-threaded version
LIBNAME=sort

# Testcases directory
TEST_SUBDIRS = testcases

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
