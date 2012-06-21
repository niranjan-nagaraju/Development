BASE_DIR=../..

INCLUDE +=-I$(PWD)

LIBNAME=sll
LIBNAME_MT=sll_mt

TESTDIR=testcases

SRC = sll.c sll_node.c

TEST_SRC = sll_node_test.c

# Pointers are truncated to 32-bit in SL for some weird reason. So switch to -m32
CFLAGS += -m32

LDFLAGS += -lpthread -m32

LIBS += -L$(OUTDIR) -l $(LIBNAME_MT)

include $(BASE_DIR)/build/GlobalMake
