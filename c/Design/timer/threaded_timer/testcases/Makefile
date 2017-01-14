BASE_DIR=../../../..

TEST_SRCS += test_timer.c
TEST_SRCS += test_multiple_timers.c

TEST_INCLUDE += -I$(BASE_DIR)/Design/timer/threaded_timer/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Design/timer/threaded_timer/out/linux/ -lthreaded_timer -lpthread

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
