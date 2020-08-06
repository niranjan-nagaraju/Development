BASE_DIR=../../..

# Source directory for the library
SRC = bst_iterator.cc
SRC2 = bst_iterator2.cc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))

	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))


runtests:
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ ./$(OBJDIR)/$(basename $(SRC2))
	@ echo 'Tests complete!'

