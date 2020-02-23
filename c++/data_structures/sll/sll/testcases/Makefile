BASE_DIR=../../../..
C_BASE_DIR = $(BASE_DIR)/../c
C_LIB_DIR = $(C_BASE_DIR)/Data_Structures/sll/

#TEST_SRCS += sll_destroy_test.c  
#TEST_SRCS += sll_find_test.c 
TEST_SRCS += sll_init_test.cpp
TEST_SRCS += sll_insert_test.cpp
#TEST_SRCS += sll_print_test.c
#TEST_SRCS += sll_remove_test.c 

#TEST_COMMON_SRCS += sll_test_common.c

TEST_INCLUDE += -I./include
TEST_INCLUDE += -I../../sll/include
TEST_INCLUDE += -I../../node/include
TEST_INCLUDE += -I$(C_LIB_DIR)/sll/include -I$(C_LIB_DIR)/node/include
TEST_INCLUDE += -I$(C_BASE_DIR)/common/include
#TEST_INCLUDE += -I$(BASE_DIR)/test_common/include

LIBS +=-L../../sll/$(OBJDIR) -lsll+
LIBS +=-L../../node/$(OBJDIR) -lsll_node+
LIBS +=-L$(C_LIB_DIR)/node/$(OBJDIR) -lsll_node
LIBS +=-L$(C_LIB_DIR)/sll/$(OBJDIR) -lsll

# library flags
#LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

# Test library flags; testcases will be linked with both LDFLAGS and TEST_LDFLAGS
#TEST_LDFLAGS += -L$(BASE_DIR)/test_common/$(OBJDIR) -ltest_common


include $(BASE_DIR)/build/Makefile.inc
