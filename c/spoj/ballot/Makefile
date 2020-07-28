BASE_DIR=../..

# Source directory for the library
SRC = ballot.c

CFLAGS += -g -ggdb

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))



runtests:
	@ cat sample.in | ./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

