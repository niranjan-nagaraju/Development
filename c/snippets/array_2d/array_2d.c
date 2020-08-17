#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>

int **
allocate_2d_array(int rows, int cols)
{
	int **arr = 0;
	int i;

	arr = (int **)malloc(rows * sizeof(int *));
	if (!arr)
		return 0;

	for (i=0; i<rows; i++) {
		arr[i] = (int *)malloc(cols * sizeof(int));
		if (!arr[i]) {
			/* couldn't allocate row i, 
			 * deallocate all previous allocations and return a NULL pointer
			 */
			while (i--)
				free(arr[i]);
			free(arr);
			return 0;
		}
	}
	
	return arr;
}


int
main(void)
{
	int **arr = allocate_2d_array(5, 6);
	int i, j;

	if (!arr)
		return -ENOMEM;

	for (i=0; i<5; i++) {
		for (j=0; j<6; j++) {
			arr[i][j] = i+j;
			printf("%d ", arr[i][j]);
		}
		printf("\n");
	}

	for (i=0; i<5; i++) {
		for (j=0; j<6; j++) {
			assert(arr[i][j] == i+j);
		}
	}

	return 0;
}
