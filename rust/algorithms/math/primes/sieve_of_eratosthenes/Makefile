BASE_DIR=../../../../

SRC = sieve_of_eratosthenes.rs

all: run

build:
	@ mkdir -p $(OBJDIR)
	@ rustc $(SRC) -o $(OBJDIR)/$(basename $(SRC))

run: build
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

include $(BASE_DIR)/build/Makefile.inc
