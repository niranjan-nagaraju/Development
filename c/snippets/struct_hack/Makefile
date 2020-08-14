BASE_DIR=../..

# Source directory for the library
SRC = struct_hack.c

CFLAGS += -g -ggdb -Wall

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))


runtests:
	@ echo 'Testing Struct Hack'
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

