#include <sort_common.h>

void swap (int *a, int *b)
{
	swapInt(a, b);
}

void getArray(int **a, int n)
{
	int i;
	*a = malloc(sizeof(int)*n);

	printf("Array: ");
	for (i=0; i<n; i++)
		scanf("%d", (*a + i));
}

void printArray(int *a, int n)
{
	int i;

	printf("Array: ");
	for(i=0; i<n; i++)
		printf("%d ", a[i]);
	printf("\n");
}

