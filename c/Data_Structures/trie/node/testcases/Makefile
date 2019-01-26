BASE_DIR=../../../..

TEST_SRCS += test_trie_node.c


TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/trie/node/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Data_structures/trie/node/$(OBJDIR)/ -ltrie_node

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
