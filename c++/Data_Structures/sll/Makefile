BASE_DIR=../..
C_BASE_DIR = ../../../c
C_LIB_DIR = $(C_BASE_DIR)/Data_Structures/sll/

# Include files
INCLUDE += -I$(PWD)/sll -I$(C_LIB_DIR)/node -I$(C_LIB_DIR)/sll -I$(C_BASE_DIR)/common

# Library filenames, _MT for multi-threaded version
LIBNAME=sll+
LIBNAME_MT=sll+_mt

# Source directory for the library
#SRC = node/sll_node.cpp
SRC += sll/sll.cpp
#SRC += sll/sll_insert.cpp sll/sll_remove.cpp sll/sll_print.cpp
#SRC += sll/sll_find.cpp

# Testcases directory
TESTDIR = testcases

# Include files for testcases
TEST_INCLUDE += -I$(PWD)

#TEST_COMMON_FILES = sll/sll_test_common.cpp

# Testcases for single-threaded library
TEST_SRC = test_core.c
#TEST_SRC = node/sll_node_create_test.cpp
#TEST_SRC += node/sll_node_print_test.cpp

#TEST_SRC += sll/sll_init_test.cpp
#TEST_SRC += sll/sll_insert_test.cpp
#TEST_SRC += sll/sll_remove_test.cpp
#TEST_SRC += sll/sll_find_test.cpp
#TEST_SRC += sll/sll_print_test.cpp
#TEST_SRC += sll/sll_destroy_test.cpp

# Testcases for Multi-threaded library
#TEST_SRC_MT += node/sll_node_print_test_mt.c

# library flags
LDFLAGS += -lpthread -L$(C_BASE_DIR)/common/$(OUTDIR) -lcommon 

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
TEST_LDFLAGS += -L$(OUTDIR) -lsll+ -L$(C_LIB_DIR)/$(OUTDIR) -lsll # -L$(BASE_DIR)/test_common/$(OUTDIR) -ltest_common 

include $(BASE_DIR)/build/GlobalMake
