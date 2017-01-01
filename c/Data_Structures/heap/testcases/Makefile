BASE_DIR=../../..

TEST_SRCS += test_heap_indexes.c
TEST_SRCS += test_up_heap.c
TEST_SRCS += test_build_heap.c


TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/heap/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Data_structures/heap/out/linux/ -lheap

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
