OUTDIR = out/mac

SRC = day1.hs

# GHCFLAGS = -odir $(OUTDIR) -hidir $(OUTDIR)
GHCFLAGS = -fno-warn-tabs  -outputdir $(OUTDIR)

all: run

build:
	@ mkdir -p $(OUTDIR)
	@ ghc --make $(GHCFLAGS) -o $(OUTDIR)/$(basename $(SRC)) $(SRC)


run: build
	@ echo 'Running against input #1'
	@ cat test1.in | ./$(OUTDIR)/$(basename $(SRC))  # 605364
	@ echo 'Running against input #2'
	@ cat test2.in | ./$(OUTDIR)/$(basename $(SRC))  # 128397680

# run without compiling
rundirect:
	@ echo 'Running against input #1'
	@ cat test1.in | runghc $(SRC)  # 605364
	@ echo 'Running against input #2'
	@ cat test2.in | runghc $(SRC)  # 128397680

clean:
	@ rm $(OUTDIR)/*
