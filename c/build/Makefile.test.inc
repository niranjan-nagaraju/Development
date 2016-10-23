# Include current library paths to testcases
LIBS +=
LIBS_MT += 

# Common source files for testcases -- These are sources that all testcases are dependent on
# These will be linked to testcase source files
TEST_COMMON_SRCS := $(addprefix $(SRCDIR)/, $(TEST_COMMON_SRCS)) 
TEST_COMMON_OBJS = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(TEST_COMMON_SRCS))

# MT versions of the common source files
TEST_COMMON_SRCS_MT := $(addprefix $(SRCDIR)/, $(TEST_COMMON_SRCS_MT)) 
TEST_COMMON_OBJS_MT = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(TEST_COMMON_SRCS_MT))

TEST_SRCS := $(addprefix $(SRCDIR)/, $(TEST_SRCS)) 
TEST_OBJS = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(TEST_SRCS))
TEST_EXES = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%, $(TEST_SRCS))
TEST_INCLUDE +=

TEST_SRCS_MT := $(addprefix $(SRCDIR)/, $(TEST_SRCS_MT)) 
TEST_OBJS_MT = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(TEST_SRCS_MT))
TEST_EXES_MT = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%, $(TEST_SRCS_MT))
TEST_INCLUDE_MT +=

$(TEST_SUBDIRS): 
	@if [ -d `pwd`/$@ ] ; \
    then \
        $(MAKE) $(MKFLG) -C $@ MWD=$(MWD)$@/ $(MAKECMDGOALS); \
    else \
        echo "<$@>: No such directory!"; \
		exit 1; \
    fi

tests:: $(SUBDIRS) $(TEST_SUBDIRS) $(TEST_EXES) $(TEST_EXES_MT)

$(TEST_EXES): %:%.o | $(OBJDIR) 
ifdef TEST_SRCS
	$(CC) $@.o -o $@ $(LIBS) $(LDFLAGS) $(TEST_LDFLAGS) $(TEST_COMMON_OBJS)
endif

$(TEST_EXES_MT): | $(OBJDIR)
ifdef TEST_SRCS_MT
	$(CC) $@.o -o $@ $(LIBS_MT) $(LDFLAGS_MT) $(TEST_LDFLAGS_MT) $(TEST_COMMON_OBJS_MT)
endif


