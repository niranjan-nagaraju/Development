#include <sort_common.h>
#include <assert.h>

typedef void (*sortfn_t) (int a[], int n);

void
test_sort(sortfn_t sortfn)
{
	int a[] = {1, 6, 3, 4, 5, 2};
	int sorted_a[] = {1, 2, 3, 4, 5, 6};

	sortfn(a, 6);
	assert(compareIntArrays(a, 0, sorted_a, 0, 6) == TRUE);

	int b[] = {6, 5, 4, 3, 2, 1};
	sortfn(b, 6);
	assert(compareIntArrays(b, 0, sorted_a, 0, 6) == TRUE);

	int c[] = {1, 2, 3, 4, 5, 6};
	sortfn(c, 6);
	assert(compareIntArrays(c, 0, sorted_a, 0, 6) == TRUE);

	int d[] = {23, 17, 31, 7, 3, 2, 11};
	int sorted_d[] = {2, 3, 7, 11, 17, 23, 31};
	sortfn(d, 7);
	assert(compareIntArrays(d, 0, sorted_d, 0, 7) == TRUE);

	int e[] = {2,5,1,4};
	int sorted_e[] = {1,2,4,5};
	sortfn(e, 4);
	assert(compareIntArrays(e, 0, sorted_e, 0, 4) == TRUE);
}


int main()
{
	test_sort(bubble_sort);
	test_sort(bubble_sortR);
	test_sort(selection_sort);
	test_sort(insertion_sort);
	test_sort(merge_sort);
	test_sort(quick_sort);
	test_sort(abacus_sort);
	return 0;
}
