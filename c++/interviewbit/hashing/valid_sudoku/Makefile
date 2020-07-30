BASE_DIR=../../..

# Source directory for the library
SRC = valid_sudoku.cc

CXXFLAGS += -g -ggdb -Wall -std=c++11

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))

runtests:
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

