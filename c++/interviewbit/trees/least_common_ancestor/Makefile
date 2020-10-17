BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = least_common_ancestor.cc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

all:: runtests

build::
	@ mkdir -p $(OBJDIR)
	@ $(CC) $(CXXFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))

runtests: build
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'
