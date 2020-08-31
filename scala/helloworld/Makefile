OBJDIR = out
SRC = hello.scala
SRC2 = hello2.scala

all: build

build:
	@ mkdir -p $(OBJDIR)
	@ scalac -d $(OBJDIR) $(SRC)

run: build
	@ scala -cp $(OBJDIR) $(basename $(SRC))
	@ scala $(SRC2)

