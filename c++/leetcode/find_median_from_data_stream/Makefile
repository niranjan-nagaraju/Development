BASE_DIR=../..
include $(BASE_DIR)/build/Makefile.inc


CXXFLAGS = -g -ggdb -std=c++11 -Wall

# Source directory for the library
SRC = find_median_from_data_stream.cc


# override all target from Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(SRC) -o $(OBJDIR)/$(basename $(SRC))


runtests:
	@ ./$(OBJDIR)/$(basename $(SRC))
