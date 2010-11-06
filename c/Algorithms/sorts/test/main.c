#include <sort_common.h>
#include <assert.h>

int main()
{
	void (*sortfn) (int a[], int n) = NULL;
	int *a, n;
#ifdef _SELECTION_SORT_
	sortfn = selection_sort;
#elif _BUBBLE_SORT_
	sortfn = bubble_sort;
#elif _INSERTION_SORT_
	sortfn = insertion_sort;
#elif _MERGE_SORT_
	sortfn = merge_sort;
#elif _QUICK_SORT_
	sortfn = quick_sort;
#elif _BUBBLE_SORTR_
	sortfn = bubble_sortR;
#endif

	assert(sortfn != NULL);

	printf("n: ");
	scanf("%d", &n);
	
	getArray(&a, n);
	
	sortfn(a, n);
	
	printArray(a, n);

	return 0;
}
