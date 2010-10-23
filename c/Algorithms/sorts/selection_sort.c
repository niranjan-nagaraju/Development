#include <sort_common.h>

void selection_sort( int a[], int n)
{
	int i, j, min;

	printf("%s\n", __func__);

	for (i=0; i<(n-1); i++) {
		min = i;
		for (j=i+1; j<n; j++)
			if (a[j] < a[min])
				min = j;
		
		swap(&a[i], &a[min]);
	}
}
