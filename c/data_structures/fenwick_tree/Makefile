BASE_DIR=../..

# Include files
INCLUDE +=-I./ -I$(BASE_DIR)/common -g

EXE = fenwick_tree

# Library filenames, _MT for multi-threaded version
#LIBNAME=sll
#LIBNAME_MT=sll_mt

# Source directory for the library
SRC = fenwick_tree.c main.c

# Testcases directory
#TESTDIR = testcases

# Include files for testcases
#TEST_INCLUDE += -I$(BASE_DIR)/test_common -I$(PWD)/$(TESTDIR)/sll

#TEST_COMMON_FILES = sll/sll_test_common.c

# Testcases for single-threaded library
#TEST_SRC = node/sll_node_create_test.c
#TEST_SRC += node/sll_node_print_test.c
#
#TEST_SRC += sll/sll_init_test.c
#TEST_SRC += sll/sll_insert_test.c
#TEST_SRC += sll/sll_remove_test.c
#TEST_SRC += sll/sll_find_test.c
#TEST_SRC += sll/sll_print_test.c
#TEST_SRC += sll/sll_destroy_test.c

# Testcases for Multi-threaded library
#TEST_SRC_MT += node/sll_node_print_test_mt.c

# library flags
LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OUTDIR) -lcommon

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
#TEST_LDFLAGS += -L$(BASE_DIR)/test_common/$(OUTDIR) -ltest_common

include $(BASE_DIR)/build/GlobalMake
