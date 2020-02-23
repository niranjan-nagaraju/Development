#include "pairs_array_sum_sorted.h"

int main(void)
{
    int a[100], n, sum;
	int i;
	struct pairs pairs;

	scanf("%d",&n);

	for(i=0;i<n;i++) {
		scanf("%d",&a[i]);
	}

	scanf("%d",&sum);

	if(! isSorted(a,n))	{
		printf("Unsorted Array!\n");
		return 0;
	}
	
	pairs = pairs_sorted_array_sum(a, n, sum);

	if (!pairs.num_pairs) {
		printf("No pairs\n");
		return -1;
	}

	for(i=0; i<pairs.num_pairs; i++)
		printf("(%d, %d) ", pairs.pairs[i].a, pairs.pairs[i].b);

	printf("\n");

	return 0;
}

