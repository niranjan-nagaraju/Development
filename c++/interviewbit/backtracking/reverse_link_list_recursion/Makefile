BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

# Source directory for the library
SRC = reverse_link_list_recursion.cc

# override all target from Makefile.inc
all:: run

run: build
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo "Tests complete!"

build:
	@ mkdir -p $(OBJDIR)
	@ $(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	@ $(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))


