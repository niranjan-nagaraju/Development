OBJDIR = out
SRC = find_numbers_with_even_number_of_digits.rs

all: build

build:
	@ mkdir -p $(OBJDIR)
	@ rustc $(SRC) -o $(OBJDIR)/$(basename $(SRC))

run: build
	@ ./$(OBJDIR)/$(basename $(SRC))
	@ echo 'Tests complete!'

