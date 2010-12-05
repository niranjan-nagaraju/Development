LIBNAME = common
TESTDIR = tests

CFLAGS = -I$(PWD)/include
LIBS = -L./out/ -lcommon

SRC = common.c

include ../build/GlobalMake

test:
	mkdir -p out
	$(CC) $(TESTDIR)/testCommon.c -o out/testCommon $(CFLAGS) $(LIBS)
