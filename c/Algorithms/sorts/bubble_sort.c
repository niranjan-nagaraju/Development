#include <sort_common.h>

void bubble_sort( int a[], int n)
{
	int i, j, swapped = 0;

	printf("%s\n", __func__);

	for (i=0; i<n; i++) {
		for (j=0; j<n-i-1; j++) {
			if (a[j] > a[j+1]) {
				swap(&a[j], &a[j+1]);
				swapped = 1;
			}
		}
		if ( !swapped )
			return;
	}
}
