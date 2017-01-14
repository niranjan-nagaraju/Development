BASE_DIR=../../../..

TEST_SRCS += test_sll_queue.c

TEST_INCLUDE += -I./include -I../include
TEST_INCLUDE += -I../../sll/include
TEST_INCLUDE += -I../../node/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include
TEST_INCLUDE += -I$(BASE_DIR)/test_common/include

LIBS +=-L../$(OBJDIR) -lsll_queue
LIBS +=-L../../sll/$(OBJDIR) -lsll
LIBS +=-L../../node/$(OBJDIR) -lsll_node

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
TEST_LDFLAGS += -L$(BASE_DIR)/test_common/$(OBJDIR) -ltest_common


include $(BASE_DIR)/build/Makefile.inc
