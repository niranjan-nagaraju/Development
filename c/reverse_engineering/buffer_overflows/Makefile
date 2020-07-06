BASE_DIR=../..

#CFLAGS += -g -ggdb -D_FORTIFY_SOURCE=0
CFLAGS +=-fno-stack-protector # disable buffer overflow protections

# Source directory for the library
SRC = simple_buffer_overflow.c

include $(BASE_DIR)/build/Makefile.inc

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(INCLUDE) $(CFLAGS) -o $(OBJDIR)/$(basename $(SRC)) $(SRC)


