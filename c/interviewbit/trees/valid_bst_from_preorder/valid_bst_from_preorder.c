/**
https://www.interviewbit.com/problems/valid-bst-from-preorder/

Valid BST from Preorder

Problem Description
You are given a preorder traversal A, of a Binary Search Tree.
Find if it is a valid preorder traversal of a BST.


Problem Constraints
1 <= A[i] <= 10⁶
1 <= |A| <= 10⁵


Input Format
First and only argument is an integer array A denoting the pre-order traversal.


Output Format
Return an integer:
0 : Impossible preorder traversal of a BST
1 : Possible preorder traversal of a BST


Example Input
Input 1:
A = [7, 7, 10, 10, 9, 5, 2, 8]

Example Output
Output 1:
 1
*/


/**
Solution Outline:
    0. Preorder traversal visits R-l-r (Root-left-right).
    1. If A[] is the array containing the items from the pre-order traversal,
        A[0] is the root of the entire tree.
    2. Look for the right child to the right A[1:], => i.e the first item that is >= A[0]
        Let's say A[r] is that right child's index.
        Everything to the right of A[r] should be >= A[0], otherwise its not a valid BST pre-order traversal.
	3. Use a decreasing stack to store nodes. [Assumes left subtrees are <= root, right subtrees are > root)
		At any item x, and the stack containing a bunch of non-increasing items,
			If x > current stacktop,
				Pop all < x, the last < x item popped from the stack will be the root of x
				all items between root ... x are the left child of root.
				Update current root := root (popped from the stack)
				Push x on to the stack
			If x <= current stacktop,
				Check if x < current root, if it is, then A[] is not a valid pre-order traversal
		If at the end of the pass, All x > current root, then A[] is a pre-order traversal

Sample run:
            40
           /  \
          30   80
            \   \
            35   36
    A: [40, 30, 35, 80, 36]
        0   1   2   3   4

	stack: [40], root = None

	x: 30 < stacktop (40)
		x < current root? NO
		push(x)
	stack: [40, 30]

	x: 35 >= current stacktop (30)
		x < current root? NO
		pop all <= x, updating last-popped as root
			pop(30), current-root: 30
		push(x)
	stack: [40, 35]

	x: 80 >= current stacktop (35)
		x < current root(30)? NO
		pop all <= x, updating last-popped as root
			pop(35)
			pop(40), current-root: 40
		push(80)
	stack: [80]
			

	x: 36 < current stacktop (80)
		x < current root? YES => Not a valid pre-order
	return false



Sample run 2:
         4
       /   \
      2     6  
    /  \   / \
   2    3 6   7 
   
  A: [4, 2, 2, 3, 6, 6, 7]
      0  1  2  3  4  5  6

	stack: [4]
	current-root: None

	x: 2 <= stacktop (4)
		x < current-root? NO
		push(x)
	stack: [4, 2]

	x: 2 <= stacktop (2)
		x <= current-root? NO
		push(x)
	stack: [4, 2, 2]

	x: 3 > stacktop (2)
		pop all < 3
			pop(2)
			pop(2), current-root: 2
		push(x)
	stack: [4, 3]

	x: 6 > stacktop (3)
		pop all < 6
			pop(3)
			pop(4), current-root: 4
		push(x)
	stack: [6]

	x: 6 <= stacktop (6)
		x <= current-root (4)? NO
		push(x)
	stack: [6, 6]

	x: 7 > stacktop (6)
		pop all < 7
			pop(6)
			pop(6), current-root: 6
		push(x)
	stack: [7]

	End of array
	return True
*/

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>


enum Boolean {FALSE=0, TRUE};

typedef struct {
	int size;
	int tos;
	int capacity;

	// struct hack to use a variable-length
	// array for stack items
	int items[0]; 
}stack_s;

stack_s *
create_stack(int capacity)
{
	stack_s *stack = (stack_s *) malloc (sizeof(stack_s) + sizeof(int)*capacity);
	if (!stack)
		return 0;

	stack->capacity = capacity;
	stack->size = 0;
	stack->tos = -1;
	return stack;
}

void
destroy_stack(stack_s *stack)
{
	free(stack);
}

void
push(stack_s *stack, int val)
{
	if (!stack || stack->size == stack->capacity)
		return;

	stack->items[++stack->tos] = val;
	stack->size++;
}

/** 
 * WARN: unchecked access
 * check if the stack is not empty
 * before calling
 */
int
top(stack_s *stack)
{
	return stack->items[stack->tos];
}

int
pop(stack_s *stack)
{
	if (!stack || !stack->size)
		return -EINVAL;

	stack->size--;
	return stack->items[stack->tos--];
}


/**
 * @input A : Integer array
 * @input n : Integer array's ( A ) length
 * 
 * @Output Integer
 */
int
is_valid_preorder_traversal(int* A, int n)
{
	stack_s *stack = create_stack(n);
	int i, current_root;

	if (!A || !stack)
		return FALSE;

	current_root = -1;
	push(stack, A[0]);

	for (i=1; i<n; i++) {
		if (A[i] <= top(stack)) {
			if (current_root != -1 && A[i] <= current_root)
				return FALSE;
		} else { // A[i] > top(stack)
			// pop all < A[i] from the stack
			while (stack->size && (top(stack) < A[i]))
				current_root = pop(stack);
		}
		push(stack, A[i]);
	}

	destroy_stack(stack);
	return TRUE;
}



int
main(void)
{
	{
		int A[] = {40, 30, 35, 80, 100};
		assert( is_valid_preorder_traversal(A, sizeof(A)/sizeof(int)) == TRUE);
	}
	{
		int A[] = {40, 30, 35, 80, 36};
		assert( is_valid_preorder_traversal(A, sizeof(A)/sizeof(int)) == FALSE);
	}
	{
		int A[] = {4, 2, 2, 3, 6, 6, 7};
		assert( is_valid_preorder_traversal(A, sizeof(A)/sizeof(int)) == TRUE);
	}
	{
		int A[] = {4, 1, 2, 3, 6, 5, 7};
		assert( is_valid_preorder_traversal(A, sizeof(A)/sizeof(int)) == TRUE);
	}
	return 0;
}

