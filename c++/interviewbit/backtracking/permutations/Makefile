BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC1 = permutations.cc

CXXFLAGS += -I$(PWD) -g -ggdb -std=c++14

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC1) -o $(OBJDIR)/$(basename $(SRC1)).o
	$(CC) $(OBJDIR)/$(basename $(SRC1)).o -o $(OBJDIR)/$(basename $(SRC1))

run: all
	@./$(OBJDIR)/$(basename $(SRC1))
	@ echo 'Tests complete!'

