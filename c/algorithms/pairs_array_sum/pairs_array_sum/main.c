#include "pairs_array_sum.h"

int main(void)
{
	struct pairs pairs;
	int a[100], n, sum;
	int i;

	scanf("%d", &n);
	
	for(i=0; i<n; i++)
		scanf("%d", &a[i]);

	scanf("%d", &sum);

	pairs = pairs_array_sum(a, n, sum);

	if (!pairs.num_pairs) {
		printf("No pairs\n");
		return -1;
	}

	for(i=0; i<pairs.num_pairs; i++)
		printf("(%d, %d) ", pairs.pairs[i].a, pairs.pairs[i].b);

	printf("\n");

	return 0;
}
