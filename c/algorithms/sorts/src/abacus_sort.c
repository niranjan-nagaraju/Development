#include <sort_common.h>

/**
 * Abacus sort - also known as bead sort/gravity sort for integers
 * O(n) space, O(mn) time, (n: size of array, m: max - min)
 *
 * Sample run:
 *  Array: 2 5 1 4
 * 
 * min: 1, max: 5
 * a: 1 1 1 1
 * 
 * i: 1 to 5
 *  j: 3 to 0
 * 
 * i: 1
 *   j: 3
 *    4 > 1
 *    a: 1 1 1 2
 *  
 *   j: 2
 *    1 > 1? NO
 * 
 *   j: 1
 *    5 > 1
 *    a: 1 1 2 2
 * 
 *   j: 0
 *    2 > 1
 *    a: 1 2 2 2
 * 
 * 
 * i: 2
 *  j: 3
 *   4 > 2
 *   a: 1 2 2 3
 * 
 *  j: 2
 *   1 > 2? NO
 *  
 *  j: 2
 *   5 > 2
 *   a: 1 2 3 3
 * 
 *  j: 0
 *   2 > 2? NO 
 * 
 * 
 * i: 3
 *   j: 3
 *    4 > 3
 *    a: 1 2 3 4
 * 
 *   j: 2
 *    1 > 3? NO
 * 
 *   j: 1
 *    5 > 3
 *    a: 1 2 4 4
 * 
 *   j: 0
 *    2 > 3? NO
 * 
 * 
 * i: 4
 *   j: 3
 *    4 > 4? NO
 * 
 *   j: 2
 *    1 > 4? NO
 * 
 *   j: 1
 *    5 > 4
 *    a: 1 2 4 5
 * 
 *   j: 0
 *    2 > 4? NO
 * 
 * 
 * i: 5
 *   j: 3
 *     4 > 5? NO
 * 
 *   j: 2
 *    1 > 5? NO
 * 
 *   j: 1
 *    5 > 5? NO
 * 
 *   j: 0
 *    2 > 5? NO
 * 
 * a: 1 2 4 5
 */
void
abacus_sort(int a[], int n)
{
    int i, j;

	/** k holds the next position in the array which
	  * is to be incremented next
	  */
    int k;
    int max, min;
    int temp[n];

	/** Find Minimum and Maximum elements in the array */
    max = min = a[0];
    for(i=1; i<n; i++) {
		/** a[i] cannot be min and max at the same time */
        if(a[i] > max)
            max = a[i];
		else if(a[i] < min)
            min = a[i];
    }

	/**
	 * Initialize Temp array all with minimum element of the
	 * array to be sorted
	 */
    for(i=0; i<n; i++) {
        temp[i] = min;
    }

    for(i=min; i<max; i++) {
		/** scan R-L for ascending; L-R for descending order */
        k = (n-1);
        for(j=k; j>=0; j--) {
			/** a[j] is still relatively large at iteration 'i'.. So increment it */
            if(a[j] > i) {
                temp[k]++;
                k--;
            }

            /**
             *  else a[j] is relatively smaller at iteration 'i'.. So it's already at
             *  it's proper place; No modifications reqd for a[j].
             */
        }
    }

    for(i=0; i<n; i++) {
        a[i] = temp[i];
    }
}
