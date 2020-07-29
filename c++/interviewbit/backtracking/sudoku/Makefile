BASE_DIR=../../..

# Source directory for the library
SRC = sudoku.cc
SRC2 = sudoku_2.cc
SRC3 = sudoku_3.cc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))

	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))

	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC3) -o $(OBJDIR)/$(basename $(SRC3)).o
	$(CC) $(OBJDIR)/$(basename $(SRC3)).o -o $(OBJDIR)/$(basename $(SRC3))


runtests:
	@ echo 'Testing Solution #1'
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Testing Solution #2'
	@./$(OBJDIR)/$(basename $(SRC2))
	@ echo 'Testing Solution #3'
	@./$(OBJDIR)/$(basename $(SRC3))
	@ echo 'Tests complete!'

