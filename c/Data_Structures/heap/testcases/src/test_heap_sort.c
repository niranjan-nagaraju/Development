#include <heap.h>
#include <assert.h>

int main(void)
{
	int l[] = {4,5,6,7,8,1,2,3,10,9};

	heap_sort(l, 10);

	printIntArray(l, 10);

	return 0;
}
