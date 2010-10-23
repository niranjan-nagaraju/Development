CFLAGS += -I$(PWD)/include -g -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast

LIBNAME = common
TESTDIR = tests

all:
	mkdir -p out
	$(CC) $(LIBNAME).c $(CFLAGS) -c -fPIC -Wall -o out/$(LIBNAME).o

dylib: all
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBNAME).dylib out/$(LIBNAME).o -lgcc

so: all
	$(CC) -shared -fPIC -o out/lib$(LIBNAME).so out/$(LIBNAME).o -lgcc

test:
	mkdir -p out
	$(CC) testCommon.c -o out/testCommon $(CFLAGS)

clean:
	rm -rf out
