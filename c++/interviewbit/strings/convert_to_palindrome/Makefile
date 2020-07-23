BASE_DIR=../../..

# Source directory for the library
SRC_1 = convert_to_palindrome.cpp
SRC_2 = convert_to_palindrome_efficient.cpp

CXXFLAGS += -g -ggdb -std=c++11

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC_1) -o $(OBJDIR)/$(basename $(SRC_1)).o
	$(CC) $(OBJDIR)/$(basename $(SRC_1)).o -o $(OBJDIR)/$(basename $(SRC_1))

	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC_2) -o $(OBJDIR)/$(basename $(SRC_2)).o
	$(CC) $(OBJDIR)/$(basename $(SRC_2)).o -o $(OBJDIR)/$(basename $(SRC_2))


run_tests:
	@ ./$(OBJDIR)/$(basename $(SRC_1))
	@ ./$(OBJDIR)/$(basename $(SRC_2))

