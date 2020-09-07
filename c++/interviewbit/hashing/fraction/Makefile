export BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = fraction.cc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))


run:
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo "Tests Complete!"

