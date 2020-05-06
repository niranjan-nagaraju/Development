BASE_DIR=../../

# Source directory for the library
SRC = palindrome_string.c

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) -o $(OBJDIR)/$(basename $(SRC)) $(SRC)

