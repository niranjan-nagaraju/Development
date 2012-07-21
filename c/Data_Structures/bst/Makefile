BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD) -I$(BASE_DIR)/common/include

# Library filenames, _MT for multi-threaded version
LIBNAME=bst
LIBNAME_MT=bst_mt

# Source directory for the library
SRC = bst.c bst_node.c

# Testcases directory
TESTDIR = testcases

# Testcases for single-threaded library
TEST_SRC = node/bst_node_create_test.c
TEST_SRC += node/bst_node_print_test.c
#TEST_SRC += bst/insert/bst_insert_test.c

# Testcases for Multi-threaded library
#TEST_SRC_MT += node/sll_node_print_test_mt.c

# library flags
LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OUTDIR) -lcommon

include $(BASE_DIR)/build/GlobalMake
