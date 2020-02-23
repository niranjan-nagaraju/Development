BASE_DIR=../../..
C_BASE_DIR = $(BASE_DIR)/../c
C_LIB_DIR = $(C_BASE_DIR)/Data_Structures/sll/

# Include files
INCLUDE += -I./include -I../node/include
INCLUDE += -I$(C_LIB_DIR)/node/include
INCLUDE += -I$(C_LIB_DIR)/sll/include -I$(C_BASE_DIR)/common/include

# Library filenames, _MT for multi-threaded version
LIBNAME=sll+
LIBNAME_MT=sll+_mt

TEST_SUBDIRS = testcases # testcases_mt

LDFLAGS += -L$(C_LIB_DIR)/sll/$(OBJDIR)/ -lsll
LDFLAGS +=-L$(C_LIB_DIR)/node/$(OBJDIR)/ -lsll_node

# library flags
#LDFLAGS += -lpthread -L$(C_BASE_DIR)/common/$(OUTDIR) -lcommon 

include $(BASE_DIR)/build/Makefile.inc
