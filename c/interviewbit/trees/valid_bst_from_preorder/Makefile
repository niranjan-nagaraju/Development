BASE_DIR=../../..

include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = valid_bst_from_preorder.c

CFLAGS = -g -ggdb -Wall

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))

runtests:
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

