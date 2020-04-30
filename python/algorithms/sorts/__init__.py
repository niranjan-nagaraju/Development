# Make all the sort functions available at the 'sorts' package level
# so individual sort modules needn't be imported
# just to access the sort methods
# from algorithms.sorts import abacus_sort
#  Vs
# from algorithms.sorts.abacus_sort import abacus_sort

from abacus_sort import abacus_sort
from merge_sort import merge_sort
from heap_sort import heap_sort
from insertion_sort import insertion_sort
from comparison_counting_sort import comparison_counting_sort
from quick_sort import quick_sort
