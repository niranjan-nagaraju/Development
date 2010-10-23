#include <sort_common.h>

void insertion_sort( int a[], int n)
{
	int i, j, key;

	printf("%s\n", __func__);

	for (i=1; i<n; i++) {
		key = a[i];
		j = i - 1;

		while ((j >= 0) && (key < a[j])) {
			a[j+1] = a[j];
			j--;
		}
		a[j+1] = key;
	}
}
