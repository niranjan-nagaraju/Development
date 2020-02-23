BASE_DIR=../../..

TEST_SUBDIRS = testcases

# Include files
INCLUDE +=-I./include -I$(BASE_DIR)/common/include
INCLUDE +=-I../sll/include -I../node/include

# Library filenames, _MT for multi-threaded version
LIBNAME=sll_queue
LIBNAME_MT=sll_queue_mt

LDFLAGS += -L$(BASE_DIR)/Data_Structures/sll/sll/$(OBJDIR)/ -lsll
LDFLAGS +=-L$(BASE_DIR)/Data_Structures/sll/node/$(OBJDIR)/ -lsll_node

include $(BASE_DIR)/build/Makefile.inc
