
BASE_DIR=../..

SRC = test_offsetOf.c

include $(BASE_DIR)/build/Makefile.inc

all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(SRC) -o $(OBJDIR)/$(basename $(SRC))
