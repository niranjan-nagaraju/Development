BASE_DIR=../..

INCLUDE +=-I$(PWD) -I$(BASE_DIR)/common/include

LIBNAME=sll
LIBNAME_MT=sll_mt

SRC = sll.c sll_node.c

TESTDIR = testcases
TEST_SRC = node/sll_node_create_test.c
TEST_SRC += node/sll_node_print_test.c
TEST_SRC += sll/insert/sll_insert_test.c

# TEST_SRC_MT += node/sll_node_print_test_mt.c

LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OUTDIR) -lcommon

LIBS += -L$(OUTDIR) -l $(LIBNAME)
LIBS_MT += -L$(OUTDIR) -l $(LIBNAME_MT)

include $(BASE_DIR)/build/GlobalMake
