export BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = modular_expression.cc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

all:: run

build::
	@ mkdir -p $(OBJDIR)
	@ $(CC) $(CXXFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))


run: build
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo "Tests complete!"

