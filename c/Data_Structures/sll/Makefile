BASE_DIR=../..
INCLUDE += -I$(PWD) -I../include -I$(BASE_DIR)/common/include 

LIBNAME = sll
LIBNAME_MT = sll_mt
TESTDIR = testcases

TEST_SRC = core/test_core.c
#TEST_SRC += display/test_display.c
#TEST_SRC += insert/test_insert.c insert/test_insertMT.c
#TEST_SRC += join/test_join.c loop/test_loop.c
#TEST_SRC += remove/test_remove.c
#TEST_SRC += reverse/test_reverse.c
#TEST_SRC += sort/test_sort.c


SRC = sll_core.c sll_insert.c sll_remove.c sll_display.c
SRC += sll_sort.c sll_reverse.c sll_shiftRotate.c
SRC += sll_set.c sll_join.c sll_loop.c
SRC += sll_node.c

include $(BASE_DIR)/build/GlobalMake

LDFLAGS += -L$(BASE_DIR)/common/$(OUTDIR) -lcommon -lpthread

tests: 
	for f in $(TEST_SRC); do \
		$(CC) $(TESTDIR)/$$f $(CFLAGS) -o $(OUTDIR)/`basename $$f .c` -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS); \
	done
