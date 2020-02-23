#include <search.h>
#include <assert.h>

int 
main(void)
{
	int arr[] = {5,4,3,2,1};

	assert(linear_search(arr, 5, 1, 0) == 4);
	assert(linear_search(arr, 5, 3, 0) == 2);
	assert(linear_search(arr, 5, 6, 0) == -1);

	return 0;
}
