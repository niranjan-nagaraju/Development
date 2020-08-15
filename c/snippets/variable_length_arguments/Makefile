BASE_DIR=../..

include $(BASE_DIR)/build/Makefile.inc

# Source files to compile
SRC1 = variable_length_arguments.c
SRC2 = printf.c

CFLAGS = -g -ggdb -Wall

all::
	@ mkdir -p $(OBJDIR)
	@ $(CC) $(CFLAGS) $(INCLUDE) -c $(SRC1) -o $(OBJDIR)/$(basename $(SRC1)).o
	@ $(CC) $(OBJDIR)/$(basename $(SRC1)).o -o $(OBJDIR)/$(basename $(SRC1))

	@ $(CC) $(CFLAGS) $(INCLUDE) -c $(SRC2) -o $(OBJDIR)/$(basename $(SRC2)).o
	@ $(CC) $(OBJDIR)/$(basename $(SRC2)).o -o $(OBJDIR)/$(basename $(SRC2))


runtests:
	@ echo 'Testing Variable length arguments'
	@./$(OBJDIR)/$(basename $(SRC1))
	@ echo 'Testing printf'
	@./$(OBJDIR)/$(basename $(SRC2))
	@ echo 'Tests complete!'

