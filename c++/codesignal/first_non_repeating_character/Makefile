# Build target using 
# make exe

BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD)

CXXFLAGS += -g -ggdb -std=c++11

# Source directory for the library
SRC = first_non_repeating_character.cpp
SRC2 = first_non_repeating_character_2.cpp


include $(BASE_DIR)/build/Makefile.inc

# override all target from Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))


