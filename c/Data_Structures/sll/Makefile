BASE_DIR=../..
CFLAGS += -I$(PWD) -g -I../include -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast 
INCLUDE += -I. -I$(BASE_DIR)/common/include 
LIBS += -L$(BASE_DIR)/common/out -lcommon -lpthread

OBJS = out/sll_core.o
OBJS += out/sll_insert.o out/sll_remove.o out/sll_display.o out/sll_sort.o 
OBJS += out/sll_reverse.o out/sll_shiftRotate.o out/sll_set.o 
OBJS += out/sll_join.o out/sll_loop.o

LIBNAME = sll
LIBMTNAME = sll_mt
TESTDIR = testcases

OS=$(shell uname)

all:
	mkdir -p out
	$(CC) $(LIBNAME)_core.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_core.o $(INCLUDE) 
	$(CC) $(LIBNAME)_insert.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_insert.o
	$(CC) $(LIBNAME)_remove.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_remove.o
	$(CC) $(LIBNAME)_display.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_display.o
	$(CC) $(LIBNAME)_sort.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_sort.o
	$(CC) $(LIBNAME)_reverse.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_reverse.o
	$(CC) $(LIBNAME)_shiftRotate.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_shiftRotate.o
	$(CC) $(LIBNAME)_set.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_set.o
	$(CC) $(LIBNAME)_join.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_join.o
	$(CC) $(LIBNAME)_loop.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/$(LIBNAME)_loop.o

libs:
ifeq ($(OS), Darwin)
	make dylib
	make dylib_mt
else
ifeq ($(OS), Linux)
	make so
	make so_mt
else
	@echo 'Unknown Operating System'
	exit 1;
endif
endif

dylib: all
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBNAME).dylib $(OBJS) -lgcc $(INCLUDE) $(LIBS)

dylib_mt: CFLAGS += -D_MULTI_THREADED_
dylib_mt: all
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBMTNAME).dylib $(OBJS) -lgcc $(INCLUDE) $(LIBS)

so: all
	$(CC) -shared -fPIC -o out/lib$(LIBNAME).so $(OBJS) -lgcc $(INCLUDE) $(LIBS)

so_mt: CFLAGS += -D_MULTI_THREADED_
so_mt: all
	$(CC) -shared -fPIC -o out/lib$(LIBMTNAME).so $(OBJS) -lgcc $(INCLUDE) $(LIBS)

tests: 
	$(CC) $(TESTDIR)/test_core.c $(CFLAGS) -o out/test_core -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_insert.c $(CFLAGS) -o out/test_insert -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_remove.c $(CFLAGS) -o out/test_remove -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_display.c $(CFLAGS) -o out/test_display -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_sort.c $(CFLAGS) -o out/test_sort -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_reverse.c $(CFLAGS) -o out/test_reverse -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_shiftRotate.c $(CFLAGS) -o out/test_shiftRotate -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_set.c $(CFLAGS) -o out/test_set -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_join.c $(CFLAGS) -o out/test_join -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)
	#$(CC) $(TESTDIR)/test_loop.c $(CFLAGS) -o out/test_loop -L$(PWD)/out/ -l$(LIBNAME) -l$(LIBNAME) $(INCLUDE) $(LIBS)

clean:
	rm -rf out
