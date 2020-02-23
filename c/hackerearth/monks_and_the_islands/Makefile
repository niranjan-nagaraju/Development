# Build target using 
# make exe

BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD)

CFLAGS += -g -ggdb

# Source directory for the library
SRC = monks_and_the_islands.c


include $(BASE_DIR)/build/Makefile.inc

# override all target from Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))


runtests:
	@ cat sample.in | $(OBJDIR)/$(basename $(SRC))

