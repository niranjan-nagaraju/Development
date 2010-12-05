BASE_DIR=../..
INCLUDE += -I$(PWD) -I../include -I$(BASE_DIR)/common/include 

LIBNAME = sll
LIBNAME_MT = sll_mt
TESTDIR = testcases

SRC = sll_core.c sll_insert.c sll_remove.c sll_display.c
SRC += sll_sort.c sll_reverse.c sll_shiftRotate.c
SRC += sll_set.c sll_join.c sll_loop.c
SRC += sll_node.c

include $(BASE_DIR)/build/GlobalMake

LDFLAGS += -L$(BASE_DIR)/common/$(OUTDIR) -lcommon -lpthread

tests: 
	$(CC) $(TESTDIR)/core/test_core.c $(CFLAGS) -o $(OUTDIR)/test_core -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_insert.c $(CFLAGS) -o out/test_insert -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_remove.c $(CFLAGS) -o out/test_remove -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_display.c $(CFLAGS) -o out/test_display -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_sort.c $(CFLAGS) -o out/test_sort -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_reverse.c $(CFLAGS) -o out/test_reverse -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_shiftRotate.c $(CFLAGS) -o out/test_shiftRotate -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_set.c $(CFLAGS) -o out/test_set -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_join.c $(CFLAGS) -o out/test_join -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)
	#$(CC) $(TESTDIR)/test_loop.c $(CFLAGS) -o out/test_loop -L$(OUTDIR) -l$(LIBNAME) $(INCLUDE) $(LDFLAGS)

