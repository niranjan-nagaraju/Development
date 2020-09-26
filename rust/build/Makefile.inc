# INFO: Include this at the end of each Makefile in individual SUBDIRS

# Directories paths

# define binaries for scala-compiler, linker and the archive program
RUSTC = rustc

OS = $(shell uname)
# switch output directory based on Operating System
ifeq ($(OS), Darwin) # MAC OS X
	OBJDIR = $(OUTDIR)/mac
else
ifeq ($(OS), Linux) # Linux
	OBJDIR = $(OUTDIR)/linux
endif
endif

# object files and binaries are created in 'linux_x86' folders within their respective SUBDIRS
SRCDIR = src
OUTDIR = out
SRCS = $(wildcard $(SRCDIR)/*.rs)

$(OBJDIR): 
	@ mkdir -p $@

# find all 'linux_x86' folders created and delete them
clean:
	@ rm -rf $(OBJDIR)

# Clean the object files generated for this system
cleanobj:
	rm -rf $(OBJDIR)/*.o

# Clean everything ever compiled.
cleanall:
	rm -rf $(OUTDIR)

