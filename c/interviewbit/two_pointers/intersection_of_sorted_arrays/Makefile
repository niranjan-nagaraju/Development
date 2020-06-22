BASE_DIR=../../../

# Source directory for the library
SRC = intersection_of_sorted_arrays.c

LDFLAGS = -L$(BASE_DIR)/common/$(OBJDIR) -lcommon
INCLUDE = -I$(BASE_DIR)/common/include/

include $(BASE_DIR)/build/Makefile.inc
all::
	@ mkdir -p $(OBJDIR)
	$(CC) $(INCLUDE) $(LDFLAGS) -o $(OBJDIR)/$(basename $(SRC)) $(SRC)

