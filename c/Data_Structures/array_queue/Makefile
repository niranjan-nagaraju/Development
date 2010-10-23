CFLAGS += -g -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast 
INCLUDE += -I. -I../../common/include 
LIBS += -L../../common/out -lcommon

LIBNAME = array_queue
TESTDIR = tests

all:
	mkdir -p out
	$(CC) $(LIBNAME).c $(CFLAGS) -c -fPIC -Wall -o out/$(LIBNAME).o

dylib: all
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBNAME).dylib out/$(LIBNAME).o -lgcc

so: all
	$(CC) -shared -fPIC -o out/lib$(LIBNAME).so out/$(LIBNAME).o -lgcc

testQueue:
	$(CC) $(TESTDIR)/$@.c $(CFLAGS) -o out/$@ -L$(PWD)/out/ -l$(LIBNAME) $(INCLUDE) $(LIBS)

testQMT: 
	$(CC) $(TESTDIR)/$@.c $(CFLAGS) -o out/$@ -L$(PWD)/out/ -l$(LIBNAME) $(INCLUDE) $(LIBS)

clean:
	rm -rf out
