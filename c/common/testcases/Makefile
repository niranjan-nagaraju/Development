BASE_DIR=../..

TEST_SRCS += test_boolean.c
TEST_SRCS += test_common.c
TEST_SRCS += test_comparators.c

TEST_INCLUDE += -I./include -I../include

LIBS +=-L../$(OBJDIR) -lcommon

# library flags
LDFLAGS += 

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
TEST_LDFLAGS += 


include $(BASE_DIR)/build/Makefile.inc
