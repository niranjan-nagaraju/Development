#include <sort_common.h>

void merge_sort_helper(int a[], int lb, int ub);
void merge(int a[], int b[], int la, int lb);

void merge_sort (int a[], int n)
{
	merge_sort_helper(a, 0, n-1);
}


void merge_sort_helper (int *a, int lb, int ub)
{
	int mid;

	if (lb == ub)
		return;

	mid = (lb + ub)/2;

	merge_sort_helper(a, lb, mid);
	merge_sort_helper(a, mid+1, ub);

	merge((a+lb), (a+mid+1), (mid-lb+1), (ub-mid));
}


void merge (int *a, int *b, int la, int lb)
{
	int i, j, k;
	int temp_array[la+lb];

	i = j = k = 0;
	while ((i < la) && (j < lb)) {
		if (a[i] < b[j])
			temp_array[k++] = a[i++];
		else
			temp_array[k++] = b[j++];
	}

	while (i < la)
		temp_array[k++] = a[i++];

	while (j < lb)
		temp_array[k++] = b[j++];

	/** Copy temporary array back to a[] and b[] in order */
	for (k=0; k<la; k++)
		a[k] = temp_array[k];

	for (j=0; j<lb; j++)
		b[j] = temp_array[k++];
}
