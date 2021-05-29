from algorithms.sorts import *
import random

def test_sortfn(sortfn):
	a = [5,4,3,2,1]
	assert sortfn(a) == [1,2,3,4,5]
	x = [random.randint(0, 10000) for _ in xrange(1000)]
	assert sortfn(x) == sorted(x)


if __name__ == '__main__':
	test_sortfn(abacus_sort)
	test_sortfn(merge_sort)
	test_sortfn(heap_sort)
	test_sortfn(insertion_sort)
	test_sortfn(comparison_counting_sort)
	test_sortfn(quick_sort)
	test_sortfn(counting_sort)
	test_sortfn(radix_sort)

