BASE_DIR=../../..

# Source directory for the library
SRC = sudoku.c
SRC2 = sudoku_2.c
SRC3 = sudoku_3.c

CFLAGS += -g -ggdb -Wall

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))

	$(CC) $(CFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))

	$(CC) $(CFLAGS) $(INCLUDE) -c $(SRC3) -o $(OBJDIR)/$(basename $(SRC3)).o
	$(CC) $(OBJDIR)/$(basename $(SRC3)).o -o $(OBJDIR)/$(basename $(SRC3))


runtests:
	@ echo 'Testing Solution #1'
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Testing Solution #2'
	@./$(OBJDIR)/$(basename $(SRC2))
	@ echo 'Testing Solution #3'
	@./$(OBJDIR)/$(basename $(SRC3))
	@ echo 'Tests complete!'

