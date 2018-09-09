BASE_DIR=../../../..
CBASE_DIR=$(BASE_DIR)/../c

#TEST_SRCS += sll_node_print_test.c
TEST_SRCS += sll_node_create_test.cpp  

TEST_INCLUDE += -I./include
TEST_INCLUDE += -I../../sll/include
TEST_INCLUDE += -I../../node/include
TEST_INCLUDE += -I$(CBASE_DIR)/Data_Structures/sll/node/include/

LIBS +=-L../$(OBJDIR) -lsll_node+

# library flags
LDFLAGS += -L$(CBASE_DIR)/Data_Structures/sll/node/$(OBJDIR) -lsll_node

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
#TEST_LDFLAGS += -L$(BASE_DIR)/test_common/$(OBJDIR) -ltest_common


include $(BASE_DIR)/build/Makefile.inc
