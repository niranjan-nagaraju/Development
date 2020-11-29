BASE_DIR=../../../

SRC1 = permutations.rs
SRC2 = permutations2.rs

all: run

build:
	@ mkdir -p $(OBJDIR)
	rustc $(SRC1) -o $(OBJDIR)/$(basename $(SRC1))
	rustc $(SRC2) -o $(OBJDIR)/$(basename $(SRC2))

run: build
	@ echo 'Running Tests'
	@ ./$(OBJDIR)/$(basename $(SRC1))
	@ ./$(OBJDIR)/$(basename $(SRC2))
	@ echo 'Tests complete!'

include $(BASE_DIR)/build/Makefile.inc
