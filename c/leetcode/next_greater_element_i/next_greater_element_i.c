/**
https://leetcode.com/problems/next-greater-element-i/

496. Next Greater Element I
You are given two arrays (without duplicates) nums1 and nums2 where nums1’s elements are subset of nums2. Find all the next greater numbers for nums1's elements in the corresponding places of nums2.

The Next Greater Number of a number x in nums1 is the first greater number to its right in nums2. If it does not exist, output -1 for this number.

Example 1:
Input: nums1 = [4,1,2], nums2 = [1,3,4,2].
Output: [-1,3,-1]
Explanation:
    For number 4 in the first array, you cannot find the next greater number for it in the second array, so output -1.
	For number 1 in the first array, the next greater number for it in the second array is 3.
	For number 2 in the first array, there is no next greater number for it in the second array, so output -1.

Example 2:
Input: nums1 = [2,4], nums2 = [1,2,3,4].
Output: [3,-1]
Explanation:
    For number 2 in the first array, the next greater number for it in the second array is 3.
	For number 4 in the first array, there is no next greater number for it in the second array, so output -1.

Note:
All elements in nums1 and nums2 are unique.
The length of both nums1 and nums2 would not exceed 1000.
*/

/**
 Solution outline
    0. Initialize nge = [-1]*n
		nge : [-1, -1, -1, ..., -1]
	1. Use a stack
	2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
		also record nge[x] = array[i]
	3. Push i onto stack
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
nextGreaterElement(int* nums1, int nums1Size, int* nums2, int nums2Size, int* returnSize)
{
	int i;
	Stack s;
	int max_nums2;
	int *nge2_hash = NULL;


	/** Empty list -- return NULL */
    if (nums1Size == 0) {
        *returnSize = 0;
        return NULL;
    }

	/** Get max of nums2[] to create a hash table indexed by numbers in nums2 */
	max_nums2 = nums2[i];
	for (i=1; i<nums2Size; i++) {
		if (nums2[i] > max_nums2)
			max_nums2 = nums2[i];
	}

	/** array containing NGEs for nums2 */
	nge2_hash = malloc(sizeof(int)*(max_nums2+1));
	assert(nge2_hash != NULL);

	for (i=0; i<=max_nums2; i++) {
		nge2_hash[i] = -1;
	}

	init_stack(&s, nums2Size);
	for (i=0; i<nums2Size; i++) {
		/** pop items off stack as long as they are < current element */	
		while (!isEmpty(&s) && top(&s) < nums2[i]) {
			int x = pop(&s);
			nge2_hash[x] = nums2[i];
		}

		/** push current element */
		push(&s, nums2[i]);
	}
	destroy_stack(&s);

	/** Create a new NGE array for answering queries from nums1 */
	int *nge = malloc(sizeof(int)*nums1Size);
	assert(nge != NULL);
	for (i=0; i< nums1Size; i++) {
		nge[i] = nge2_hash[nums1[i]];
	}

	*returnSize = nums1Size;
	return nge;
}


