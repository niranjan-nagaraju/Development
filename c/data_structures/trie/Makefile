BASE_DIR=../..

SUBDIRS = node

# Include files
INCLUDE +=-I./include -I$(BASE_DIR)/common/include
INCLUDE +=-I./node/include 
INCLUDE += -I$(BASE_DIR)/Data_Structures/sll/queue/include
INCLUDE += -I$(BASE_DIR)/Data_Structures/sll/sll/include
INCLUDE += -I$(BASE_DIR)/Data_Structures/sll/node/include

# Library filenames, _MT for multi-threaded version
LIBNAME=trie
#LIBNAME_MT=trie_mt


LDFLAGS += -L$(BASE_DIR)/Data_Structures/sll/sll/$(OBJDIR)/ -L$(BASE_DIR)/Data_Structures/sll/node/$(OBJDIR)/
LDFLAGS += -L$(BASE_DIR)/Data_Structures/sll/queue/$(OBJDIR)/ -lsll_queue

TEST_SUBDIRS = testcases #testcases_mt

include $(BASE_DIR)/build/Makefile.inc
