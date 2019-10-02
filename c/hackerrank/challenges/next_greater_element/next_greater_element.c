/**
https://www.hackerrank.com/contests/second/challenges/next-greater-element

Given an array, print the Next Greater Element (NGE) for every element. The Next greater Element for an element x is the first greater element on the right side of x in array. Elements for which no greater element exist, consider next greater element as -1.

For the input array [4, 5, 2, 25}, the next greater elements for each element are as follows.

Element --> NGE
4 --> 5
5 --> 25
2 --> 25
25 --> -1

For the input array [13, 7, 6, 12}, the next greater elements for each element are as follows.

Element --> NGE
13 --> -1
7 --> 12
6 --> 12
12 --> -1


Input Format
The first line of input contains an integer n denoting the size of the array
The next line contains n space seperated array elements in integer range
0 < n < = 65535

Output Format
Output consists of n lines
Each line should contain 2 space seperated integers
The first integer should represent the array element and second integer should represent the next greater element

Sample Input
4
4 5 2 25

Sample Output
4 5
5 25
2 25
25 -1
*/


/**
Solution outline
    0. Initialize nge = [-1]*n
		nge : [-1, -1, -1, ..., -1]
	1. Use a stack and solve the problem of next-greater-element like matching parantheses.
	2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
		also record nge[x] = array[i]
	3. Push i onto stack	
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
	int *items;
	int tos;
} Stack;


void
init_stack(Stack *s, int n)
{
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
	s->tos += 1;
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


int*
next_greater_element(int array[], int n)
{
	Stack s;
	int i;
	int *nge;

	nge = malloc(sizeof(int)*n);
	assert(nge != NULL);

	for (i=0; i<n; i++) {
		nge[i] = -1;
	}

	init_stack(&s, n);
	for (i=0; i<n; i++) {
		while (!isEmpty(&s) && array[top(&s)] < array[i]) {
			int x = pop(&s);
			nge[x] = array[i];
		}
		push(&s, i);
	}

	destroy_stack(&s);
	return nge;
}


void
test_next_greater_element(int array[], int expected_nge[], int n)
{
	int i;
	int *nge = next_greater_element(array, n);
	for (i=0; i<n; i++)
		assert (nge[i] == expected_nge[i]);

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

	int nge_a[] = {2,3,4,-1};
	int nge_b[] = {4,2,4,-1};
	int nge_c[] = {5,25,25,-1};
	int nge_d[] = {-1,12,12,-1};
	int nge_e[] = {-1,-1,-1,-1};

	test_next_greater_element(a, nge_a, sizeof(a)/sizeof(int));
	test_next_greater_element(b, nge_b, sizeof(b)/sizeof(int));
	test_next_greater_element(c, nge_c, sizeof(c)/sizeof(int));
	test_next_greater_element(d, nge_d, sizeof(d)/sizeof(int));
	test_next_greater_element(e, nge_e, sizeof(e)/sizeof(int));

	return 0;
}
