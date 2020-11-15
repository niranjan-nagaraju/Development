BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC1 = combinations_sum.cc
SRC2 = combinations_sum2.cc

CXXFLAGS += -g -ggdb -std=c++11

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC1) -o $(OBJDIR)/$(basename $(SRC1)).o
	$(CC) $(OBJDIR)/$(basename $(SRC1)).o -o $(OBJDIR)/$(basename $(SRC1))
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))

run: all
	@./$(OBJDIR)/$(basename $(SRC1))
	@./$(OBJDIR)/$(basename $(SRC2))
	@ echo 'Tests complete!'

