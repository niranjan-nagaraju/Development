/**
https://leetcode.com/problems/next-greater-element-ii/

503. Next Greater Element II

Given a circular array (the next element of the last element is the first element of the array), print the Next Greater Number for every element. The Next Greater Number of a number x is the first greater number to its traversing-order next in the array, which means you could search circularly to find its next greater number. If it doesn't exist, output -1 for this number.

Example 1:
Input: [1,2,1]
Output: [2,-1,2]
Explanation: The first 1's next greater number is 2; 
The number 2 can't find next greater number; 
The second 1's next greater number needs to search circularly, which is also 2.
Note: The length of given array won't exceed 10000.
*/

/**
 Solution outline
    0. Initialize nge = [-1]*n
		nge : [-1, -1, -1, ..., -1]
	1. Use a stack and solve the problem of next-greater-element like matching parantheses.
	2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
		also record nge[x] = array[i]
	3. Push i onto stack
	4. Since its a circular array, traverse the array again (after reaching the end), until the penultimate item.
       a = [1,2,3,4]
	   1,2,3,4,1,2,3 covers the circular array.
	5. Fill nge[x] only if it has not been found yet.
	   This ensures that a previously found nge[] is  not overwritten by a new one in the circular traversal,
	   as the new one is certainly 'farther'.
		e.g. [5,1,2,3,4]
	    nge[1] should not be overwritten by 5, and should remain 2
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
	int *items;
	int tos;
	int max_size;
} Stack;


void
init_stack(Stack *s, int n)
{
	s->max_size = n;
	s->items = malloc(sizeof(int)*n);
	assert(s->items != NULL);
	s->tos = -1;
}

void
destroy_stack(Stack *s)
{
	assert(s->items != NULL);
	free(s->items);
	s->items = 0;
	s->tos = -1;
}


/** 
 * Unsafe push - doesnt check for allocations
 * Caller is expected to check for these
 */
void
push(Stack *s, int item)
{
	s->tos++;
	assert(s->tos < s->max_size);
	s->items[s->tos] = item;
}


/** 
 * Unsafe pop - doesnt check for allocations/NULL pointers
 * Caller is expected to check for these
 */
int
pop(Stack *s)
{
	return s->items[s->tos--];
}

int
isEmpty(Stack *s)
{
	if (!s || !s->items)
		return 1;

	return (s->tos == -1) ? 1 : 0;
}

/**
 * Return top of the stack
 * Unsafe: caller is expected to check if the stack is empty
 */
int
top(Stack *s)
{
	return s->items[s->tos];
}


/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
int*
nextGreaterElements(int *nums, int numsSize, int* returnSize)
{
	Stack s;
	int i;
	int *nge;

	nge = malloc(sizeof(int)*numsSize);
	assert(nge != NULL);

	for (i=0; i<numsSize; i++) {
		nge[i] = -1;
	}

	/** 
	 * Since circular array is traversed from start->finish,
	 * => linearly traversing the array twice,
	 * and hence needs 2x the stack size
	 */
	init_stack(&s, numsSize*2);
	for (i=0; i<numsSize; i++) {
		while (!isEmpty(&s) && nums[top(&s)] < nums[i]) {
			int x = pop(&s);
			nge[x] = nums[i];
		}
		push(&s, i);
	}

	// Circular traversal, traverse till the penultimate item this time
	for (i=0; i<numsSize-1; i++) {
		while (!isEmpty(&s) && nums[top(&s)] < nums[i]) {
			int x = pop(&s);
			// if NGE has already been found for the current item
			// Ignore it for this pass
			if (nge[x] == -1)
				nge[x] = nums[i];
		}
		push(&s, i);
	}
	destroy_stack(&s);

	*returnSize = numsSize;
	return nge;
}


void
test_next_greater_element(int array[], int expected_nge[], int n)
{
	int i, ret;
	int *nge = nextGreaterElements(array, n, &ret);

	assert (ret == n);
	for (i=0; i<n; i++) {
		assert (nge[i] == expected_nge[i]);
	}

	free(nge);
}


int
main(void)
{
	int a[] = {1,2,3,4};
	int b[] = {3,1,2,4};
	int c[] = {4,5,2,25};
	int d[] = {13,7,6,12};
	int e[] = {4,3,2,1};
	int f[] = {1,2,1};
	int g[] = {1,1,1,1,1};

	int nge_a[] = {2,3,4,-1};
	int nge_b[] = {4,2,4,-1};
	int nge_c[] = {5,25,25,-1};
	int nge_d[] = {-1,12,12,13};
	int nge_e[] = {-1,4,4,4};
	int nge_f[] = {2,-1,2};
	int nge_g[] = {-1,-1,-1,-1,-1};

	test_next_greater_element(g, nge_g, sizeof(g)/sizeof(int));
	test_next_greater_element(a, nge_a, sizeof(a)/sizeof(int));
	test_next_greater_element(b, nge_b, sizeof(b)/sizeof(int));
	test_next_greater_element(c, nge_c, sizeof(c)/sizeof(int));
	test_next_greater_element(d, nge_d, sizeof(d)/sizeof(int));
	test_next_greater_element(e, nge_e, sizeof(e)/sizeof(int));
	test_next_greater_element(f, nge_f, 3);

	return 0;
}
