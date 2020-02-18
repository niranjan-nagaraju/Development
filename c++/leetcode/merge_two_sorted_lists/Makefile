# Build target using 
# make exe

BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD)

CXXFLAGS += -g -ggdb -std=c++11

# Source directory for the library
SRC = merge_two_sorted_lists.cpp


include $(BASE_DIR)/build/Makefile.inc

# override all target from Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))


