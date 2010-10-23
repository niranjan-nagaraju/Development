CFLAGS += -g -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast 
INCLUDE += -I. -I../../common/include 
LIBS += -L../../common/out -lcommon

OBJS = out/bst.o

LIBNAME = bst
TESTDIR = testcases

all:
	mkdir -p out
	$(CC) $(LIBNAME).c $(CFLAGS) -c -fPIC -Wall -o out/$(LIBNAME).o

dylib: all
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBNAME).dylib out/$(LIBNAME).o -lgcc

so: all
	$(CC) -shared -fPIC -o out/lib$(LIBNAME).so out/$(LIBNAME).o -lgcc

testTree: 
	$(CC) $(TESTDIR)/$@.c $(CFLAGS) -o out/$@ -L$(PWD)/out/ -l$(LIBNAME) $(INCLUDE) $(LIBS)

clean:
	rm -rf out
