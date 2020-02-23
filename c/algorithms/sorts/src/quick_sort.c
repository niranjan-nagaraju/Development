#include <sort_common.h>

int partition(int a[], int lb, int ub);
void quick_sort_helper(int a[], int lb, int ub);

void quick_sort(int a[], int n)
{
	quick_sort_helper(a, 0, n-1);
}

void quick_sort_helper(int a[], int lb, int ub)
{
	int p;

	if (lb >= ub)
		return;

	p = partition(a, lb, ub);
	quick_sort_helper(a, lb, p-1);
	quick_sort_helper(a, p+1, ub);
}


int partition(int a[], int lb, int ub)
{
	int p = a[lb];
	int i, j;

	i = lb;
	for(j=lb+1; j<=ub; j++) {
		if (a[j] < p) {
			i++;
			swapInt(&a[i], &a[j]);
		}
	}
	swapInt(&a[lb], &a[i]);

	return i;
}
