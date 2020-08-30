SRC = hello.scala
OBJDIR = out

all: build

build:
	@ mkdir -p $(OBJDIR)
	scalac -d $(OBJDIR) $(SRC)

run: build
	scala -cp $(OBJDIR) $(basename $(SRC))
