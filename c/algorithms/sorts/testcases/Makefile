BASE_DIR=../../..

TEST_SRCS += test_sorts.c

CFLAGS += -I../include


TEST_INCLUDE += -I./include
TEST_INCLUDE += -I$(BASE_DIR)/common/include
TEST_INCLUDE += -I$(BASE_DIR)/test_common/include

LIBS +=-L../$(OBJDIR) -lsort

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
TEST_LDFLAGS += -L$(BASE_DIR)/test_common/$(OBJDIR) -ltest_common


include $(BASE_DIR)/build/Makefile.inc
