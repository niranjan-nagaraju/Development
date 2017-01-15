BASE_DIR=../../..

TEST_SRCS += test_trie.c
TEST_SRCS += trie_find_words_matching_prefix.c
TEST_SRCS += interactive_trie.c


TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/trie/include
TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/trie/node/include
TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/sll/queue/include
TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/sll/sll/include
TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/sll/node/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Data_structures/trie/$(OBJDIR)/ -ltrie
LIBS +=-L$(BASE_DIR)/Data_structures/trie/node/$(OBJDIR)/ -ltrie_node
LIBS +=-L$(BASE_DIR)/Data_structures/sll/queue/$(OBJDIR)/ -lsll_queue
LIBS +=-L$(BASE_DIR)/Data_structures/sll/sll/$(OBJDIR)/ -lsll
LIBS +=-L$(BASE_DIR)/Data_structures/sll/node/$(OBJDIR)/ -lsll_node

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
