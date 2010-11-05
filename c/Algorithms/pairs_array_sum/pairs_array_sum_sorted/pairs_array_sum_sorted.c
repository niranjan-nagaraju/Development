#include "pairs_array_sum_sorted.h"

int
isSorted (int a[], int n)
{
	int i;

	for (i=0; i<n-1; i++)
		if (a[i] > a[i+1])
			return 0;

	return 1;
}

struct pairs 
pairs_sorted_array_sum(int a[] , int n, int sum)
{
	struct pairs pairs; 
	int i, j, k;
	int currsum;

	pairs.num_pairs=0;

	i=0; 
	j=n-1; 
	k=0; 
	while (i<j) {
		currsum = a[i]+a[j];

		if (currsum == sum) {
			   pairs.num_pairs++;

               pairs.pairs[k].a = a[i];
               pairs.pairs[k].b = a[j];
               k++;
			   i++;
		} else if (currsum > sum) {
			j--;
		} else {
			i++;
        }
	}

	return pairs;
}
