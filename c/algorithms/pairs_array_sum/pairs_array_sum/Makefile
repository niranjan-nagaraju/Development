CFLAGS += -I$(PWD) -g

CC = gcc
SRC = pairs_array_sum.c main.c
EXE = pairs_array_sum

all:
	mkdir -p out
	$(CC) $(SRC) $(CFLAGS) -o out/$(EXE)

clean:
	    rm -rf out
