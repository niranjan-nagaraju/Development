#include <search.h>
#include <common.h>


int
linear_search(void *list, int n, void *key, comparefn compare)
{
	int i;

	/** Assume integers if no compare function is provided */
	if (!compare)
		compare = compareInts;

	for(i=0; i<n; i++) {
		if(compare(list, i, key, -1) == 0) {
			return i;
		}
	}

	return -1;
}
