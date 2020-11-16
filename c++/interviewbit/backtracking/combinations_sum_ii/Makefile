BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = combinations_sum_ii.cc

CXXFLAGS += -g -ggdb -std=c++11

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))

run: all
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

