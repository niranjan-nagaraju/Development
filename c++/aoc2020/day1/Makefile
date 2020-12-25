BASE_DIR=../..
include $(BASE_DIR)/build/Makefile.inc

# Source directory for the library
SRC = day1.cc

CXXFLAGS += -g -ggdb -Wall -std=c++14

all:: build

build:
	@ mkdir -p $(OBJDIR)
	$(CC) $(CXXFLAGS) $(INCLUDE) -c $(SRC) -o $(OBJDIR)/$(basename $(SRC)).o
	$(CC) $(OBJDIR)/$(basename $(SRC)).o -o $(OBJDIR)/$(basename $(SRC))

run: all
	@ echo "Running tests against input #1"
	@ cat test1.in | ./$(OBJDIR)/$(basename $(SRC)) # 514579, 241861950
	@ echo "Running tests against input #2"
	@ cat test2.in | ./$(OBJDIR)/$(basename $(SRC)) # 605364, 128397680

