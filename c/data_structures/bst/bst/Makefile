BASE_DIR=../../..

SUBDIRS = testcases

# Include files
INCLUDE +=-I./include -I../node/include -I$(BASE_DIR)/common/include
INCLUDE +=-I$(BASE_DIR)/Data_Structures/array/array_queue/include 

# Library filenames, _MT for multi-threaded version
LIBNAME=bst
LIBNAME_MT=bst_mt

LDFLAGS += -L$(BASE_DIR)/Data_Structures/array/array_queue/$(OBJDIR)/ -larray_queue

include $(BASE_DIR)/build/Makefile.inc
