BASE_DIR=../..

#CFLAGS += -g -ggdb
CFLAGS +=-fno-stack-protector # disable buffer overflow protections

# Source directory for the library
SRC = out_of_bounds_write.c

include $(BASE_DIR)/build/Makefile.inc

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(INCLUDE) $(CFLAGS) -o $(OBJDIR)/$(basename $(SRC)) $(SRC)


