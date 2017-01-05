BASE_DIR=../../..

TEST_SRCS += test_trie.c


TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/trie/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Data_structures/trie/out/linux/ -ltrie

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
