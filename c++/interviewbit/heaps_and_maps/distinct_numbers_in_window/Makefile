BASE_DIR=../../..

# Source file
SRC = distinct_numbers_in_window.cc

CXXFLAGS += -std=c++11 -g -ggdb -Wall

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))



runtests:
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

