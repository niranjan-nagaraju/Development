#include <sort_common.h>


/**
 * Bubble sort:
 *   O(n^2) time, O(1) time complexity
 *   Moves the greatest element to the last index at iteration 0,
 *   ith greatest at iteration (n-i), 
 *   by repeatedly swapping the greatest item seen in this pass to the right.
 *
 * Sample run:
 * a: {1, 6, 3, 4, 5, 2};
 *
 * Iteration 1:
 *   a: 1 3 4 5 2 6 
 * Iteration 2:
 *   a: 1 3 4 2 5 6 
 * Iteration 3:
 *   a: 1 3 2 4 5 6 
 * Iteration 4:
 *   a: 1 2 3 4 5 6 
 * Iteration 5:
 *   a: 1 2 3 4 5 6 
 * Iteration 6:
 *   a: 1 2 3 4 5 6
 */
void
bubble_sort( int a[], int n)
{
	int i, j, swapped = 0;

	for (i=0; i<n; i++) {
		for (j=0; j<n-i-1; j++) {
			if (a[j] > a[j+1]) {
				swap(&a[j], &a[j+1]);
				swapped = 1;
			}
		}

		/**
		 * If nothing was swapped in the current iteration
		 * the array is in sorted order
		 * and no more passes are needed
		 */
		if ( !swapped )
			return;
	}
}


static void _bubble_sortR( int a[], int n);

/** recursive version of the bubble sort algorithm */
void
bubble_sortR(int a[], int n)
{
	_bubble_sortR(a, n);
}


/** Helper function to recursively bubble-sort the specified array */
static void
_bubble_sortR( int a[], int n)
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


