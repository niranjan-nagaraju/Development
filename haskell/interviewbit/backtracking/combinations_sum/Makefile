OUTDIR = out/mac

SRC = combinations_sum.hs

# GHCFLAGS = -odir $(OUTDIR) -hidir $(OUTDIR)
GHCFLAGS = -outputdir $(OUTDIR)

all: run

build:
	@ mkdir -p $(OUTDIR)
	@ ghc --make $(GHCFLAGS) -o $(OUTDIR)/$(basename $(SRC)) $(SRC)

run: build
	@ ./$(OUTDIR)/$(basename $(SRC))

# run without compiling
rundirect:
	@ runghc $(SRC)

clean:
	@ rm $(OUTDIR)/*
