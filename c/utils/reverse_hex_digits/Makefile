BASE_DIR=../../

EXE = test_reverse_hex_digits
override SRCDIR=.

CFLAGS += -I.
LDFLAGS += $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(SRCS))

all:: libs exe

include $(BASE_DIR)/build/Makefile.inc

