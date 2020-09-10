export BASE_DIR=../../..

# Source directory for the library
SRC = letter_phone.scala
CLASS = Solution

all:: run

build:
	@ mkdir -p $(OBJDIR)
	$(SCALAC) -d $(OBJDIR) $(SRC)

run: build
	scala -cp $(OBJDIR) $(CLASS)
	@ echo "Tests complete!"

rundirect: # run without compiling
	scala $(SRC)
	@ echo "Tests complete!"

include $(BASE_DIR)/build/Makefile.inc
