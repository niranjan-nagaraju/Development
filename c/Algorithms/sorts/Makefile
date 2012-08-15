BASE_DIR=../..

# Include files
INCLUDE +=-I$(PWD) -I$(BASE_DIR)/common

# Library filenames, _MT for multi-threaded version
LIBNAME=sort

# Source directory for the library
SRC = sort_common.c
SRC += bubble_sort.c bubble_sortR.c
# SRC += counting_sort.c
# SRC += heap_sort.c              
SRC += insertion_sort.c
SRC += merge_sort.c             
SRC += quick_sort.c    
# SRC += radix_sort.c             
SRC += selection_sort.c

# Testcases directory
TESTDIR = testcases

# Testcases for single-threaded library
TEST_SRC = test_sorts.c

# library flags
LDFLAGS += -lpthread -L$(BASE_DIR)/common/$(OUTDIR) -lcommon

include $(BASE_DIR)/build/GlobalMake
