# Build target using 
# make exe

BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD)

CXXFLAGS += -g -ggdb -std=c++11 -Wall

# Source directory for the library
SRC1 = singleton_ptr.cpp
SRC2 = singleton_ref.cpp


include $(BASE_DIR)/build/Makefile.inc

# override all target from Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC1) -o $(OBJDIR)/$(basename $(SRC1)).o
	$(CC) $(OBJDIR)/$(basename $(SRC1)).o -o $(OBJDIR)/$(basename $(SRC1))
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))

run_tests:
	@ ./$(OBJDIR)/$(basename $(SRC1))
	@ ./$(OBJDIR)/$(basename $(SRC2))



