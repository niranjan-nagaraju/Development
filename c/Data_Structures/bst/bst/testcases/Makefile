BASE_DIR=../../../..

TEST_SRCS += bst_insert_test.c
TEST_SRCS += bst_print_test.c

TEST_INCLUDE += -I./include
TEST_INCLUDE += -I../../bst/include
TEST_INCLUDE += -I../../node/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include
TEST_INCLUDE += -I$(BASE_DIR)/test_common/include

LIBS +=-L../../bst/out/linux/ -lbst
LIBS +=-L../../node/out/linux/ -lbst_node
LIBS +=-L$(BASE_DIR)/Data_Structures/array_queue/out/linux/ -larray_queue

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
TEST_LDFLAGS += -L$(BASE_DIR)/test_common/$(OBJDIR) -ltest_common


include $(BASE_DIR)/build/Makefile.inc
