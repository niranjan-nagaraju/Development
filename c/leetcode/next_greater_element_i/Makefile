# Build target using 
# make exe

BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD)

CFLAGS += -g -ggdb

# Source directory for the library
SRC = next_greater_element_i.c

TESTSRC = test_next_greater_element_i.c

include $(BASE_DIR)/build/Makefile.inc

# override all target from Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/next_greater_element_i.o
	$(CC) $(CFLAGS) $(INCLUDE) -c $(TESTSRC) -o $(OBJDIR)/test_next_greater_element_i.o
	$(CC) $(OBJDIR)/next_greater_element_i.o $(OBJDIR)/test_next_greater_element_i.o -o $(OBJDIR)/next_greater_element_i

runtests: all
	@ ./$(OBJDIR)/next_greater_element_i
