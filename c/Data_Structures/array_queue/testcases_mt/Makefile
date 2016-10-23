BASE_DIR=../../..

TEST_SRCS_MT += testQMT.c


TEST_INCLUDE += -I./../include
TEST_INCLUDE += -I$(BASE_DIR)/common/include

LIBS_MT +=-L../out/linux/ -larray_queue_mt

# library flags
LDFLAGS_MT += -L$(BASE_DIR)/common/$(OBJDIR) -lcommon


include $(BASE_DIR)/build/Makefile.inc
