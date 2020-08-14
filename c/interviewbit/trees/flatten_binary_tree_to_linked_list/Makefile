BASE_DIR=../../..

include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = flatten_binary_tree_to_linked_list.c

CFLAGS = -g -ggdb -Wall

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))

runtests:
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

