BASE_DIR=../../..

include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = populate_next_right_pointers.c

CFLAGS = -g -ggdb -Wall

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))

runtests:
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

