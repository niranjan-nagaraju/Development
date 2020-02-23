BASE_DIR=../../../..

TEST_SRCS_MT += sll_insert_test.c 

TEST_INCLUDE += -I./include
TEST_INCLUDE += -I../../sll/include
TEST_INCLUDE += -I../../node/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include
TEST_INCLUDE += -I$(BASE_DIR)/test_common/include

LIBS_MT +=-L../../sll/$(OBJDIR) -lsll_mt
LIBS_MT +=-L../../node/$(OBJDIR) -lsll_node

# library flags
LDFLAGS_MT += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
TEST_LDFLAGS_MT += -L$(BASE_DIR)/test_common/$(OBJDIR) -ltest_common ../testcases/$(OBJDIR)/sll_test_common.o


include $(BASE_DIR)/build/Makefile.inc
