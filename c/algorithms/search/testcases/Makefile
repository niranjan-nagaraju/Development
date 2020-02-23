BASE_DIR=../../..

TEST_SRCS += test_linear_search.c


TEST_INCLUDE += -I$(BASE_DIR)/Algorithms/search/include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS +=-L$(BASE_DIR)/Algorithms/search/out/linux/ -lsearch

# library flags
LDFLAGS += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon

include $(BASE_DIR)/build/Makefile.inc
