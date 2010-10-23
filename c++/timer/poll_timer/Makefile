CFLAGS += -I$(PWD) -g 

LIBNAME = timer
TESTDIR = tests
CC = g++

all:
	mkdir -p out
	$(CC) $(LIBNAME).cpp $(CFLAGS) -c -fPIC -Wall -o out/$(LIBNAME).o

dylib: all
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBNAME).dylib out/$(LIBNAME).o -lpthread

so: all
	$(CC) -shared -fPIC -o out/lib$(LIBNAME).so out/$(LIBNAME).o -lpthread

test_timer:
	$(CC) $(TESTDIR)/$@.cpp $(CFLAGS) -o out/$@ -L$(PWD)/out/ -l$(LIBNAME)

clean:
	rm -rf out
