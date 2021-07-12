BASE_DIR=../..

# Source directory for the library
SRC = simple_generator.cc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))


runtests:
	@ echo 'Testing Simple generator'
	@./$(OBJDIR)/$(basename $(SRC))

