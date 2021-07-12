BASE_DIR=../..

# Source directory for the library
SRC = simple_generator.cc
SRC2 = simple_generator2.cc

CXXFLAGS += -g -ggdb -std=c++14 -Wall

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))

	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))

runtests:
	@ echo 'Testing Simple generator using class'
	@./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Testing Simple generator using lambda'
	@./$(OBJDIR)/$(basename $(SRC2))

