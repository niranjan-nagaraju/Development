BASE_DIR=../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = struct_hack.c

CFLAGS = -g -ggdb -Wall

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))


runtests:
	@ echo 'Testing Struct Hack'
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

