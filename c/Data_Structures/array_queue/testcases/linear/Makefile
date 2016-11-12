BASE_DIR=../../../..

#TEST_SRCS += testErase.c
#TEST_SRCS += testLength.c
#TEST_SRCS += testPeek.c
#TEST_SRCS += testQMT.c
TEST_SRCS += testQueueInt.c
#TEST_SRCS += testResize.c


TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/array_queue/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Data_structures/array_queue/out/linux/ -larray_queue

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
