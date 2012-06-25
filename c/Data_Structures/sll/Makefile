BASE_DIR=../..

INCLUDE +=-I$(PWD) -I$(BASE_DIR)/common/include

LIBNAME=sll
LIBNAME_MT=sll_mt

SRC = sll.c sll_node.c

TESTDIR=testcases
TEST_SRC = sll_node_create_test.c
TEST_SRC += sll_node_print_test.c

LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OUTDIR) -lcommon

LIBS += -L$(OUTDIR) -l $(LIBNAME_MT)

include $(BASE_DIR)/build/GlobalMake
