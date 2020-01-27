# Build target using 
# make exe

BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD)

# Source directory for the library
SRC = next_greater_element_ii.c

include $(BASE_DIR)/build/Makefile.inc

# override all target from Makefile.inc
all::
	@echo "Building from sources"
	$(CC) $(CFLAGS) $(INCLUDE) -c next_greater_element_ii.c -o $(OBJDIR)/next_greater_element_ii.o
	$(CC) $(OBJDIR)/next_greater_element_ii.o -o $(OBJDIR)/next_greater_element_ii

runtests:
	@ ./$(OBJDIR)/next_greater_element_ii
	@ echo "Tests complete"
