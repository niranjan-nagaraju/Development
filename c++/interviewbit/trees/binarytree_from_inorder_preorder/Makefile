BASE_DIR=../../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = binarytree_from_inorder_preorder.cc

CXXFLAGS += -g -ggdb -std=c++11 -Wall

all::
	@ mkdir -p $(OBJDIR)
	@ $(CC) $(CXXFLAGS) $(INCLUDE) $(SRC) -o $(OBJDIR)/$(basename $(SRC))

runtests:
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'
