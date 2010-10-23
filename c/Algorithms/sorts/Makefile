CFLAGS += -g
INCLUDE += -I$(PWD)/include -I../../common/include
LIBS += -L../../common/out -lcommon -L./out/

LIBNAME = sort

SRC = test/main.c

# Add more sorting algorithms here
OBJS = out/sort_common.o 
OBJS += out/selection_sort.o out/bubble_sort.o out/insertion_sort.o out/merge_sort.o
OBJS += out/quick_sort.o

all:
	mkdir -p out
	$(CC) sort_common.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/sort_common.o
	$(CC) selection_sort.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/selection_sort.o
	$(CC) bubble_sort.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/bubble_sort.o
	$(CC) insertion_sort.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/insertion_sort.o
	$(CC) merge_sort.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/merge_sort.o
	$(CC) quick_sort.c $(CFLAGS) $(INCLUDE) -c -fPIC -Wall -o out/quick_sort.o

# Mac Dynamic Libray (Add to DYLD_LIBRARY_PATH)
dylib: all 	
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o out/lib$(LIBNAME).dylib $(OBJS) -lgcc $(LIBS)

# Unix Shared Library (Add to LD_LIBRARY_PATH)
so: all
	$(CC) -shared -fPIC -o out/lib$(LIBNAME).so $(OBJS) -lgcc $(LIBS)

tests:
	$(CC) -D_SELECTION_SORT_ $(SRC) $(CFLAGS) $(INCLUDE) $(LIBS) -o out/selection_sort_test -l$(LIBNAME)
	$(CC) -D_BUBBLE_SORT_ $(SRC) $(CFLAGS) $(INCLUDE) $(LIBS) -o out/bubble_sort_test -l$(LIBNAME)
	$(CC) -D_INSERTION_SORT_ $(SRC) $(CFLAGS) $(INCLUDE) $(LIBS) -o out/insertion_sort_test -l$(LIBNAME)
	$(CC) -D_MERGE_SORT_ $(SRC) $(CFLAGS) $(INCLUDE) $(LIBS) -o out/merge_sort_test -l$(LIBNAME)
	$(CC) -D_QUICK_SORT_ $(SRC) $(CFLAGS) $(INCLUDE) $(LIBS) -o out/quick_sort_test -l$(LIBNAME)

cleanobjs:
	rm -f out/*.o 

clean:
	rm -f out/*
