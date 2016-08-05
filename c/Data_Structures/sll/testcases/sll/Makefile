BASE_DIR=../../../..

TEST_SRCS += sll_destroy_test.c  
TEST_SRCS += sll_find_test.c 
TEST_SRCS += sll_init_test.c 
TEST_SRCS += sll_insert_test.c 
TEST_SRCS += sll_print_test.c
TEST_SRCS += sll_remove_test.c 

TEST_COMMON_SRCS += sll_test_common.c

TEST_INCLUDE += -I./include
TEST_INCLUDE += -I../../sll/include
TEST_INCLUDE += -I../../node/include
TEST_INCLUDE += -I$(BASE_DIR)/common
TEST_INCLUDE += -I$(BASE_DIR)/test_common

LIBS +=-L../../sll/out/linux/ -lsll
LIBS +=-L../../node/out/linux/ -lsll_node

# library flags
LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
TEST_LDFLAGS += -L$(BASE_DIR)/test_common/$(OBJDIR) -ltest_common


include $(BASE_DIR)/build/Makefile.inc
