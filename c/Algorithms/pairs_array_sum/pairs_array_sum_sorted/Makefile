CFLAGS += -I$(PWD) -g

CC = gcc
SRC = pairs_array_sum_sorted.c main.c
EXE = pairs_array_sum_sorted

all:
	mkdir -p out
	$(CC) $(SRC) $(CFLAGS) -o out/$(EXE)

clean:
	    rm -rf out
