#include <search.h>
#include <common.h>

/** TODO: Move to common */
int 
compareAtInt(void *list, int idx, void *key, comparefn compare)
{
	/** for now, assume int array */
	int *l = list;

	return compare(l[idx], key);
}

int
linear_search(void *list, int n, void *key, comparefn compare)
{
	int i;

	/** Assume integers if no compare function is provided */
	if (!compare)
		compare = compareInts;

	for(i=0; i<n; i++) {
		if(compareAtInt(list, i, key, compare) == 0) {
			return i;
		}
	}

	return -1;
}
