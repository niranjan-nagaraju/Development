OUTDIR = out/mac

SRC = permutations.hs

# GHCFLAGS = -odir $(OUTDIR) -hidir $(OUTDIR)
GHCFLAGS = -fno-warn-tabs  -outputdir $(OUTDIR)

all: run

build:
	@ mkdir -p $(OUTDIR)
	ghc --make $(GHCFLAGS) -o $(OUTDIR)/$(basename $(SRC)) $(SRC)

run: build
	@ ./$(OUTDIR)/$(basename $(SRC))

# run without compiling
rundirect:
	@ runghc $(SRC)

clean:
	@ rm $(OUTDIR)/*
