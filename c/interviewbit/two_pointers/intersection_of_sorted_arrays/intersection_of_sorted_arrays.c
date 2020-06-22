/**
https://www.interviewbit.com/problems/intersection-of-sorted-arrays/

Intersection Of Sorted Arrays

Find the intersection of two sorted arrays.
OR in other words,
Given 2 sorted arrays, find all the elements which occur in both the arrays.

Example :

Input : 
    A : [1 2 3 3 4 5 6]
    B : [3 3 5]

Output : [3 3 5]

Input : 
    A : [1 2 3 3 4 5 6]
    B : [3 5]

Output : [3 5]
*/

/**
Solution Outline:
  1. Proceed as with merging two sorted arrays
     1.1 Initialize two pointers, i and j to begin at A and B respectively
     1.2 If A[i] < B[j]:
           Move i ahead
         If A[i] > B[j]:
           Move j ahead
         If A[i] == B[j]:
           Copy A[i] to output
           Move ahead both i,j
  2. When either of A or B runs out, we are done finding the intersection of A and B, return the collected output.


Sample run:
  A: [1, 3, 3, 4, 5, 6]
  B: [2, 3, 5]

  i, j = 0
  A[i] = 1
  B[j] = 2
  A[i] < B[j] => i++
    i = 1

  A[i] = A[1] = 3
  B[j] = 2
  A[i] > B[j] => j++
    j = 1
  
  A[i] = A[1] = 3
  B[j] = B[1] = 3
  A[i] = B[j]
    Output: [3]
    i++, j++
    i = 2, j = 2

  A[i] = A[2] = 3
  B[j] = B[2] = 5
  A[i] < B[j] => i++
    i = 3

  A[i] = A[3] = 4
  B[j] = B[2] = 5
  A[i] < B[j] => i++
    i = 4

  A[i] = A[4] = 5
  B[j] = B[2] = 5
  A[i] = B[j]
    Output: [3, 5]
    i++, j++
    i = 5, j = 3

  A[i] = A[5] = 5
  B[j] = (j = len(B)): exit

  Output: [3,5]
*/
#include <assert.h>
#include <stdlib.h>

#include <common.h>
/**
 * @input A : Read only ( DON'T MODIFY ) Integer array
 * @input n1 : Integer array's ( A ) length
 * @input B : Read only ( DON'T MODIFY ) Integer array
 * @input n2 : Integer array's ( B ) length
 * 
 * @Output Integer array. You need to malloc memory, and fill the length in len1
 */
int*
find_intersection(const int* A, int n1, const int* B, int n2, int *len1)
{
	#define MAX(x, y) ((x>y) ? x:y)
	int n = MAX(n1, n2);
	int i, j, k;
	int *intersects = malloc(n*sizeof(int));

	if (!intersects)
		return 0;

	i = j = k = 0;
	while (i<n1 && j<n2) {
		if (A[i] < B[j])
			i++;
		else if (A[i] > B[j])
			j++;
		else { /* A[i] == B[j] */
			intersects[k++] = A[i];
			i++; j++;
		}
	}

	*len1 = k;
	return intersects;
}


int
main(void)
{
	{
		int A[] = {1,3,3,4,5,6};
		int B[] = {2,3,5};
		int expected_AB[] = {3,5};

		int AB_len;
		int *AB = find_intersection(A, 6, B, 3, &AB_len);
		assert(AB_len == 2);
		assert(compareIntArrays(AB, 0, expected_AB, 0, AB_len));
		free(AB);
	}
	{
		// reversing A and B should yield the same output
		int A[] = {2,3,5};
		int B[] = {1,3,3,4,5,6};
		int expected_AB[] = {3,5};

		int AB_len;
		int *AB = find_intersection(A, 3, B, 6, &AB_len);
		assert(AB_len == 2);
		assert(compareIntArrays(AB, 0, expected_AB, 0, AB_len));
		free(AB);
	}
	{
		int A[] = {1,2,3,3,4,5,6};
		int B[] = {3,3,5};
		int expected_AB[] = {3,3,5};

		int AB_len;
		int *AB = find_intersection(A, 7, B, 3, &AB_len);
		assert(AB_len == 3);
		assert(compareIntArrays(AB, 0, expected_AB, 0, AB_len));
		free(AB);
	}
	{
		int A[] = {1,2,3,3,4,5,6};
		int B[] = {3,5};
		int expected_AB[] = {3,5};

		int AB_len;
		int *AB = find_intersection(A, 7, B, 2, &AB_len);
		assert(AB_len == 2);
		assert(compareIntArrays(AB, 0, expected_AB, 0, AB_len));
		free(AB);
	}

	return 0;
}
