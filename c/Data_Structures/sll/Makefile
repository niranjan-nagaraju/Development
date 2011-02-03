# C Development root
BASE_DIR=../..

# include flags for the compiler
INCLUDE += -I$(PWD) -I../include -I$(BASE_DIR)/common/include 

# Library name
LIBNAME = sll

# Multi-threaded library name
LIBNAME_MT = sll_mt

# Testcases directory
TESTDIR = testcases

# Source files needed to compile the library
SRC = sll_core.c sll_insert.c sll_remove.c sll_display.c
SRC += sll_sort.c sll_reverse.c sll_shiftRotate.c
SRC += sll_set.c sll_join.c sll_loop.c
SRC += sll_node.c

# Define sourcefiles for testcases that need the single-threaded library
TEST_SRC = core/test_destroy.c
TEST_SRC += display/test_display.c
TEST_SRC += insert/test_insert.c insert/test_insertMT.c
TEST_SRC += join/test_join.c loop/test_loop.c
TEST_SRC += remove/test_remove.c
TEST_SRC += reverse/test_reverse.c
TEST_SRC += sort/test_sort.c

# Define sourcefiles for testcases that need the multi-threaded library
TEST_SRC_MT = insert/test_insertMT.c 

# Linker flags for dependent libraries to compile this library
LDFLAGS += -L$(BASE_DIR)/common/$(OUTDIR) -lcommon -lpthread

# Add reference to this library to be used in the testcases
LIBS += -L$(OUTDIR) -l $(LIBNAME)

# Add referece to the mult-thread version of this library to be used in the testcases
LIBS_MT += -L$(OUTDIR) -l $(LIBNAME_MT)

all: libs tests

include $(BASE_DIR)/build/GlobalMake


