BASE_DIR=../../..

TEST_SRCS += testStackInt.c


TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/array_stack/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Data_structures/array_stack/out/linux/ -larray_stack

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
