BASE_DIR=../../../..

TEST_SRCS += contacts_prefix_search.c


TEST_INCLUDE += -I$(BASE_DIR)/Data_structures/trie/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Data_structures/trie/out/linux/ -ltrie

# library flags
#LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon


solution: 
	mkdir -p $(OBJDIR)
	$(CC) src/contacts_prefix_search_full.c -o $(OBJDIR)/contacts_prefix_search_full

include $(BASE_DIR)/build/Makefile.inc
