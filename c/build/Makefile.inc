# INFO: Include this at the end of each Makefile in individual SUBDIRS

# Directories paths

# define binaries for C-compiler, linker and the archive program
CC = gcc
LD = ld
AR = ar

OS = $(shell uname)
# switch output directory based on Operating System
# Pointers are truncated to 32-bit in Snow Leopard for some weird reason. So switch to -m32
ifeq ($(OS), Darwin) # MAC OS X
	OBJDIR = $(OUTDIR)/mac
	#CFLAGS += -m32
	#LDFLAGS += -m32
else
ifeq ($(OS), Linux) # Linux
	OBJDIR = $(OUTDIR)/linux
endif
endif

CFLAGS_DEBUG += -g -Wall
CFLAGS_OPT += -O2
CFLAGS += -fPIC

# if debug is not the target, define 'opt' flags for compilation
ifeq ($(filter  debug,$(MAKECMDGOALS)),)
CFLAGS += $(CFLAGS_OPT)
endif

ifdef LIBNAME_MT
CFLAGS_MT += -pthread
CFLAGS_MT += -D_MULTI_THREADED_
LDFLAGS_MT += -lpthread
endif

ARFLAGS += 

# define INCLUDE paths and files
INCLUDE += 

# define dependent libraries and paths
LIBS +=



# object files and binaries are created in 'linux_x86' folders within their respective SUBDIRS
SRCDIR = src
OUTDIR = out
SRCS = $(wildcard $(SRCDIR)/*.c)
OBJS = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(SRCS))
OBJS_MT = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%_mt.o, $(SRCS))

.PHONY: $(SUBDIRS) 


# Recursively build object files from C source files in current directory and subdirectories defined earlier
# if LIBNAME is defined, build an archive(static library)
# Else just build object files
# NOTE: 'all' target has a :: so it can be 'overridden' in child makefiles - define all:: target in child makefiles to execute both targets
#all:: $(OBJS) $(SUBDIRS) $(LIBNAME) $(LIBNAME_MT) tests
all:: libs tests

libs: $(OBJS) $(SUBDIRS) $(LIBNAME) $(LIBNAME_MT)

$(LIBNAME): | $(OBJDIR) 
ifneq "$(LIBNAME)" ""
	$(AR) rcs $(OBJDIR)/lib$(LIBNAME).a $(OBJS)
ifeq ($(OS), Darwin) 
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o $(OBJDIR)/lib$(LIBNAME).dylib $(OBJS) -lgcc $(LDFLAGS)
else
ifeq ($(OS), Linux)
	$(CC) -shared -o $(OBJDIR)/lib$(LIBNAME).so $(LDFLAGS) -Wl,--whole-archive $(OBJDIR)/lib$(LIBNAME).a -Wl,--no-whole-archive
else
	@echo 'Unknown Operating System'
	exit 1;
endif
endif
endif # end ifneq "$(LIBNAME)" ""

$(LIBNAME_MT): | $(OBJDIR) 
ifneq "$(LIBNAME_MT)" ""
	$(AR) rcs $(OBJDIR)/lib$(LIBNAME_MT).a $(OBJS_MT)
ifeq ($(OS), Darwin) 
	$(CC) -shared -fPIC -dylib -flat_namespace -undefined suppress -o $(OBJDIR)/lib$(LIBNAME_MT).dylib $(OBJS_MT) -lgcc $(LDFLAGS) $(LDFLAGS_MT)
else
ifeq ($(OS), Linux)
	$(CC) -shared -o $(OBJDIR)/lib$(LIBNAME_MT).so $(LDFLAGS) $(LDFLAGS_MT) -Wl,--whole-archive $(OBJDIR)/lib$(LIBNAME_MT).a -Wl,--no-whole-archive
else
	@echo 'Unknown Operating System'
	exit 1;
endif
endif
endif # end ifneq "$(LIBNAME)" ""

exe: 
ifneq "$(EXE)" ""
	$(CC) $(CFLAGS) $(INCLUDE) $< -o $(OBJDIR)/$(EXE) $(LDFLAGS) 
endif # end ifneq "$(EXE)" ""


# Build for a given sub-directory	
$(SUBDIRS): 
	@if [ -d `pwd`/$@ ] ; \
    then \
        $(MAKE) $(MKFLG) -C $@ MWD=$(MWD)$@/ $(MAKECMDGOALS); \
    else \
        echo "<$@>: No such directory!"; \
		exit 1; \
    fi


# Build object files from C sources in the current working directory	
$(OBJS): $(OBJDIR)/%.o: $(SRCDIR)/%.c | $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDE) $(TEST_INCLUDE) -c $< -o $@

# Build MT versions of the object files
	$(CC) $(CFLAGS) $(CFLAGS_MT) $(INCLUDE) $(TEST_INCLUDE) -c $< -o $(basename $@)_mt.o


$(OBJDIR): 
	mkdir -p $@

debug: CFLAGS+=$(CFLAGS_DEBUG)
debug: $(SUBDIRS) $(filter-out debug,$(MAKECMDGOALS))

# 'opt' CFLAGS are on by default if debug isn't defined
opt: $(SUBDIRS) $(filter-out opt,$(MAKECMDGOALS))

# find all 'linux_x86' folders created and delete them
clean: $(SUBDIRS)
	rm -rf $(OBJDIR)

# Clean the object files generated for this system
cleanobj: $(SUBDIRS)
	rm -rf $(OBJDIR)/*.o

# Clean everything ever compiled.
cleanall: $(SUBDIRS)
	rm -rf $(OUTDIR)

include $(BASE_DIR)/Makefile.test.inc
