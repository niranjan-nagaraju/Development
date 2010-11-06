#include <sort_common.h>

static void _bubble_sortR( int a[], int n);

void bubble_sortR(int a[], int n)
{
	printf("%s\n", __func__);

	_bubble_sortR(a, n);
}

static void _bubble_sortR( int a[], int n)
{
	int i;
	int swapped = 0;

	if ( n <= 0)
		return;

	for (i=0; i<n-1; i++) {
		if (a[i] > a[i+1]) {
			swap(&a[i], &a[i+1]);
			swapped = 1;
		}
	}

	if (swapped)
		_bubble_sortR(a, n-1);
}


