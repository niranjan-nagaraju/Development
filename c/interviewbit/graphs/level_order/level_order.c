/**
https://www.interviewbit.com/problems/level-order/

Level Order

Given a binary tree, return the level order traversal of its nodes' values. (ie, from left to right, level by level).

Example :
Given binary tree

    3
   / \
  9  20
    /  \
   15   7
return its level order traversal as:

[
  [3],
  [9,20],
  [15,7]
]
Also think about a version of the question where you are asked to do a level order traversal of the tree when depth of the tree is much greater than number of nodes on a level.
*/

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/**
 * Definition for binary tree
 */
struct TreeNode {
	int val;
	struct TreeNode *left;
	struct TreeNode *right;
};

typedef struct TreeNode treenode;

treenode*
treenode_new(int val) {
	treenode* node = (treenode *) malloc(sizeof(treenode));
	node->val = val;
	node->left = NULL;
	node->right = NULL;
	return node;
}

#define MAX(a, b) (a > b) ? a : b

int
depth(treenode *root)
{
	if (!root)
		return 0;

	return MAX(depth(root->left), depth(root->right)) + 1;
}


void
level_widths(treenode *root, int level, int *widths)
{
	if (!root)
		return;

	widths[level]++;
	level_widths(root->left, level+1, widths);
	level_widths(root->right, level+1, widths);
}


void
dfs(treenode *root, int level, int *widths, int **level_order_seq)
{
	if (!root)
		return;

	level_order_seq[level][--widths[level]] = root->val;

	// DFS right-to-left so we can fill in the level right-to-left
	dfs(root->right, level+1, widths, level_order_seq);
	dfs(root->left, level+1, widths, level_order_seq);
}


/**
 * @input A : Root pointer of the tree 
 * 
 * @Output 2D int array. You need to malloc memory. Fill in len1 as row, len2 as columns 
 */
int**
levelOrder(treenode* A, int *len1)
{
	int **level_order_seq = 0;
	int d = 0;
	int *widths = 0;
	int i;

	if (!A)
		return level_order_seq;

	d = depth(A);
	widths = (int *) malloc (sizeof(int) * d);
	memset(widths, 0, sizeof(int)*d);
	level_widths(A, 0, widths);

	level_order_seq = (int **)(malloc(sizeof(int *) * d));
	for (i=0; i<d; i++) {
		level_order_seq[i] = (int *) (malloc(sizeof(int) * widths[i]));
	}

	dfs(A, 0, widths, level_order_seq);

	free(widths);

	*len1 = d;
	return level_order_seq;
}


int
main(void)
{
	int len1, i, j;
	int **level_order_seq;
	int expected[][2] =
			{					
				{3},
				{9, 20},
				{15, 7}
			};
	int widths[] = {1, 2, 2};

	/**
    3
   / \
  9  20
    /  \
   15   7
	*/
	treenode *root = treenode_new(3);
	root->left = treenode_new(9);
	root->right = treenode_new(20);
	root->right->left = treenode_new(15);
	root->right->right = treenode_new(7);

	level_order_seq = levelOrder(root, &len1);

	for (i=0; i<len1; i++) {
		for (j=0; j<widths[i]; j++) {
			assert(level_order_seq[i][j] == expected[i][j]);
		}
		free(level_order_seq[i]);
	}
	free(level_order_seq);

	return 0;
}


