#include "pairs_array_sum.h"

struct pairs
pairs_array_sum(int a[], int n, int sum)
{
	struct pairs pairs; 
	int i, j, k = 0;

	pairs.num_pairs = 0;

	for (i=0; i<n; i++) {
		for(j=i+1; j<n; j++) {
			if ((a[i] + a[j]) == sum) {
				struct pair tmp;
				pairs.num_pairs ++;
				tmp.a = a[i];
				tmp.b = a[j];

				pairs.pairs[k++] = tmp;
			}
		}
	}
	
	return pairs;
}
