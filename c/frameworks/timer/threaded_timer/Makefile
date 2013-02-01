CFLAGS += -I$(PWD) -g

LIBNAME = threaded_timer
TESTDIR = test
CC = gcc

TESTSRC = test_timer.c test_multiple_timer.c

all:
	mkdir -p out
	$(CC) $(LIBNAME).c $(CFLAGS) -c -fPIC -Wall -o out/$(LIBNAME).o

lib:
ifeq ($(shell uname), Darwin)
	make dylib
else
ifeq ($(shell uname), Linux)
	make so
else
	@echo 'Unknown OS'
	exit 1;
endif
endif

dylib: all
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBNAME).dylib out/$(LIBNAME).o -lpthread

so: all
	$(CC) -shared -fPIC -o out/lib$(LIBNAME).so out/$(LIBNAME).o -lpthread

tests:
	$(CC) $(TESTDIR)/test_timer.c $(CFLAGS) -o out/test_timer -L$(PWD)/out/ -l$(LIBNAME)
	$(CC) $(TESTDIR)/test_multiple_timers.c $(CFLAGS) -o out/test_multiple_timer -L$(PWD)/out/ -l$(LIBNAME)

clean:
	    rm -rf out
